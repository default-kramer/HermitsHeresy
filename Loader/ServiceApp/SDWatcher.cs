using HH.Core;
using NLog;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data.SQLite;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

/// <summary>
/// Watches a single SD directory.
/// </summary>
sealed class SDWatcher : IDisposable
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();
	private readonly DirectoryInfo sd;
	private readonly FileSystemWatcher watcher;
	private readonly FileInfo configFile;
	private readonly FileInfo dbFile;
	private readonly DirectoryInfo archiveRoot;
	private readonly SQLiteConnection conn;
	private readonly WorkQueue workQueue;
	private readonly CmndatManager cmndatManager = new();

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

		archiveRoot = hh.CreateSubdirectory("archive");

		conn = ConnectionFactory.OpenDatabase(dbFile);
		this.workQueue = WorkQueue.StartNewWorker();
	}

	public void Dispose()
	{
		workQueue?.Dispose();
		watcher?.Dispose();
		conn?.Dispose();
	}

	private bool ShouldProcessAsStgdat(FileSystemEventArgs e, out Func<byte[], StgdatHelper> helperFactory)
	{
		var file = new FileInfo(e.FullPath);

		// Should consult config file here... For now just hard-code it:
		if (file.Name == "STGDAT01.BIN" && file.Directory?.Name == "B02")
		{
			helperFactory = rawContent => new StgdatHelper.IoA(rawContent);
			return true;
		}
		else
		{
			helperFactory = _ => throw new Exception("Assert fail");
			return false;
		}
	}

	private readonly ConcurrentDictionary<FileVersion, bool> capturedStgdats = new();

	private void Watcher_Changed(object sender, FileSystemEventArgs e)
	{
		logger.Debug("File event: {0} {1}", e.ChangeType, e.FullPath);

		if (e.FullPath.Contains(".hermits-heresy"))
		{
			ProcessInternalFile(e);
			return;
		}

		string filename = Path.GetFileName(e.Name)?.ToUpperInvariant() ?? "";
		if (filename == "CMNDAT.BIN")
		{
			workQueue.AddWork(new Work.CaptureCmndatWork(e.FullPath, cmndatManager));
		}
		else if (ShouldProcessAsStgdat(e, out var helperFactory))
		{
			workQueue.AddWork(new Work.ArchiveStgdatWork(e.FullPath, cmndatManager, archiveRoot));
		}
	}

	private void ProcessInternalFile(FileSystemEventArgs e)
	{
		// TODO check if it is our config file and refresh it
	}
}
