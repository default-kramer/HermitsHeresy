using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data.SQLite;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

static class ConnectionFactory
{
	private static readonly ConcurrentDictionary<string, bool> openFiles = new();

	public static SQLiteConnection OpenDatabase(string path) => OpenDatabase(new FileInfo(path));

	public static SQLiteConnection OpenDatabase(FileInfo path)
	{
		if (!openFiles.TryAdd(path.FullName, true))
		{
			// For now I'm planning on opening just one connection (per DB) and keeping it open until shutdown.
			// If that ever changes I will likely need to change something here.
			throw new Exception($"Duplicate attempt to open {path.FullName}");
		}
		var connectionString = new SQLiteConnectionStringBuilder()
		{
			DataSource = path.FullName
		};
		var conn = new SQLiteConnection(connectionString.ConnectionString);
		conn.Open();
		RunMigrations(conn);
		return conn;
	}

	private static void RunMigrations(SQLiteConnection conn)
	{
		var command = conn.CreateCommand();
		command.CommandText = "create table if not exists MigrationTracker(ScriptId integer not null); select coalesce(max(ScriptId), -1) from MigrationTracker;";
		object result = command.ExecuteScalar();
		int maxScriptId = Convert.ToInt32(result);

		foreach (var script in GetScripts().Where(x => x.id > maxScriptId).OrderBy(x => x.id))
		{
			var stream = ThisAssembly.GetManifestResourceStream(script.name)
				?? throw new Exception($"How can this stream not exist? {script.name}");

			string content = new StreamReader(stream).ReadToEnd();
			try
			{
				command.CommandText = content;
				command.ExecuteNonQuery();

				int scriptId = script.id;
				command.CommandText = $"insert into MigrationTracker(ScriptId) values({scriptId})";
				command.ExecuteNonQuery();
			}
			catch (Exception ex)
			{
				throw new Exception($"Error running {script.name}, check inner exception", ex);
			}
		}
	}

	public static IEnumerable<(string name, int id)> GetScripts()
	{
		const string prefix = "ServiceApp.sql.";
		var scripts = ThisAssembly.GetManifestResourceNames()
			.Where(name => name.StartsWith(prefix))
			.OrderBy(name => name)
			.ToList();

		foreach (var script in scripts)
		{
			string filename = Path.GetFileNameWithoutExtension(script.Substring(prefix.Length));
			int id = int.Parse(filename);
			yield return (script, id);
		}
	}

	private static Assembly ThisAssembly => typeof(ConnectionFactory).Assembly;
}
