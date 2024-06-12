using HH.Core;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data.SQLite;
using System.Linq;
using System.Reflection.PortableExecutable;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

sealed class Dispatcher : IDisposable
{
	private readonly List<SDWatcher> watchers = new();

	public void Dispose()
	{
		foreach (var watcher in watchers)
		{
			watcher.Dispose();
		}
	}

	public void StartWatchers()
	{
		var dir = new DirectoryInfo("C:/Users");
		if (!dir.Exists)
		{
			throw new Exception("Can't find users dir");
		}

		foreach (var subdir in dir.EnumerateDirectories())
		{
			var sd = new DirectoryInfo(Path.Combine(subdir.FullName, "Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD"));
			if (sd.Exists)
			{
				watchers.Add(new SDWatcher(sd));
			}
		}

		if (watchers.Count == 0)
		{
			throw new Exception("Couldn't find any DQB2 saves");
		}
	}

	/// <summary>
	/// Watches a single SD directory.
	/// </summary>
	sealed class SDWatcher : IDisposable
	{
		private readonly DirectoryInfo sd;
		private readonly FileSystemWatcher watcher;
		private readonly FileInfo configFile;
		private readonly FileInfo dbFile;

		public SDWatcher(DirectoryInfo sd)
		{
			this.sd = sd;
			this.watcher = new FileSystemWatcher(sd.FullName);
			watcher.IncludeSubdirectories = true;
			watcher.EnableRaisingEvents = true;
			watcher.NotifyFilter = NotifyFilters.LastWrite;
			watcher.Filter = "*.bin";
			watcher.Changed += Watcher_Changed;
			watcher.Created += Watcher_Changed;

			var hh = sd.CreateSubdirectory(".hermits-heresy");
			configFile = new FileInfo(Path.Combine(hh.FullName, "hermits-heresy-config.json"));
			dbFile = new FileInfo(Path.Combine(hh.FullName, "hermits-heresy.db"));
		}

		public void Dispose()
		{
			watcher.Dispose();
		}

		private readonly Dictionary<string, DateTime> lastLoadTimes = new();

		private void Watcher_Changed(object sender, FileSystemEventArgs e)
		{
			// TODO reload config file from SD directory to see if we care about this...
			// But for now, just hard-code it
			if (Path.GetFileName(e.Name) == "STGDAT01.BIN")
			{
				var (writeTime, rawContent) = TryRead(e.FullPath, retryCount: 3);
				if (lastLoadTimes.TryGetValue(e.FullPath, out var lastLoadTime)
					&& writeTime <= lastLoadTime)
				{
					return;
				}

				lastLoadTimes[e.FullPath] = writeTime;

				var connectionString = new SQLiteConnectionStringBuilder()
				{
					DataSource = dbFile.FullName,
				};
				using var conn = new SQLiteConnection(connectionString.ConnectionString);
				conn.Open();
				// TODO make sure DB is up-to-date


				var stgdat = new Stgdat(new StgdatHelper.IoA(rawContent)); // TODO don't assume IoA
				new Loader(stgdat, conn).Load();
			}
		}

		private static (DateTime, byte[]) TryRead(string fullPath, int retryCount)
		{
			try
			{
				var writeTime = File.GetLastWriteTimeUtc(fullPath);
				var rawContent = File.ReadAllBytes(fullPath);
				return (writeTime, rawContent);
			}
			catch (System.IO.IOException) when (retryCount > 0)
			{
				// DQB2 still has the file locked
				System.Threading.Thread.Sleep(1000);
				return TryRead(fullPath, retryCount - 1);
			}
		}
	}
}
