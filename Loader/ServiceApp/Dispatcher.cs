using HH.Core;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data.SQLite;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

/// <summary>
/// This should be enough to uniquely identify a certain version of a certain file
/// as long as nothing really weird is happening.
/// </summary>
readonly struct FileVersion
{
	private readonly string FullPath;
	public readonly DateTime LastWriteTimeUtc;

	private FileVersion(FileInfo fileInfo, DateTime lastWriteTimeUtc)
	{
		// Store it as a string, but use FileInfo to normalize the path:
		//   var f1 = new FileInfo(@"C:\blah\foo.txt");
		//   var f2 = new FileInfo(@"C:/blah/asdf/../foo.txt");
		//   Assert.AreEqual(f1.FullName, f2.FullName);
		//   Assert.AreNotEqual(f1, f2);
		this.FullPath = fileInfo.FullName;
		this.LastWriteTimeUtc = lastWriteTimeUtc;
	}

	public FileInfo FileInfo => new FileInfo(FullPath);

	public static (FileVersion, byte[]) TryReadBytes(string fullPath, int retryCount)
	{
		try
		{
			var writeTime = File.GetLastWriteTimeUtc(fullPath);
			var rawContent = File.ReadAllBytes(fullPath);
			var snap = new FileVersion(new FileInfo(fullPath), writeTime);
			return (snap, rawContent);
		}
		catch (IOException) when (retryCount > 0)
		{
			// DQB2 still has the file locked
			Thread.Sleep(1000);
			return TryReadBytes(fullPath, retryCount - 1);
		}
	}
}

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
	}

	public void Dispose()
	{
		watcher.Dispose();
		conn.Dispose();
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

	private readonly ConcurrentDictionary<FileVersion, byte[]> capturedCmndats = new();

	/// <summary>
	/// The goal is to find the CMNDAT that belongs with the STGDAT and archive it in the same directory.
	/// </summary>
	/// <param name="StgdatVersion">A STGDAT file that has been archived</param>
	/// <param name="ArchiveDir">The archive directory</param>
	record struct CmndatArchiveRequest(FileVersion StgdatVersion, DirectoryInfo ArchiveDir);

	private readonly ConcurrentQueue<CmndatArchiveRequest> cmndatArchiveRequests = new();

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
			// always save a copy in case we need it
			var (fileVersion, rawContent) = FileVersion.TryReadBytes(e.FullPath, retryCount: 3);
			capturedCmndats[fileVersion] = rawContent;
			TryResolveRequests();
		}
		else if (ShouldProcessAsStgdat(e, out var helperFactory))
		{
			var (stgdatVersion, stgdatContent) = FileVersion.TryReadBytes(e.FullPath, retryCount: 3);

			if (capturedStgdats.TryAdd(stgdatVersion, true))
			{
				string archiveUniqueName = DateTime.Now.ToString("yyyyMMdd-HHmmss")
					+ "-" + (stgdatVersion.FileInfo.Directory?.Name ?? "WTF")
					+ "-" + Path.GetFileNameWithoutExtension(stgdatVersion.FileInfo.Name);
				var archiveDir = this.archiveRoot.CreateSubdirectory(archiveUniqueName);

				// Maybe have a dedicated worker thread for the SQL connection?
				// I'm still not sure Watcher_Changed is a single-threaded model
				// (like e.g. "the UI thread" in a Winforms app)
				/*
				var helper = helperFactory(stgdatContent);
				var stgdat = new Stgdat(helper);
				new Loader(stgdat, conn).Load();
				*/

				File.WriteAllBytes(Path.Combine(archiveDir.FullName, stgdatVersion.FileInfo.Name), stgdatContent);
				WriteInfoText(archiveDir, stgdatVersion);

				cmndatArchiveRequests.Enqueue(new CmndatArchiveRequest(stgdatVersion, archiveDir));
				TryResolveRequests();
			}
		}
	}

	private void TryResolveRequests()
	{
		TryResolveRequests(cmndatArchiveRequests, capturedCmndats.ToArray());
	}

	private static void TryResolveRequests(ConcurrentQueue<CmndatArchiveRequest> cmndatArchiveRequests,
		IEnumerable<KeyValuePair<FileVersion, byte[]>> capturedCmndats)
	{
		List<CmndatArchiveRequest> unresolvedRequests = new();

		while (cmndatArchiveRequests.TryDequeue(out var request))
		{
			if (!TryResolveRequest(request, capturedCmndats))
			{
				unresolvedRequests.Add(request);
			}
		}

		foreach (var failed in unresolvedRequests)
		{
			const int timeoutMinutes = 5; // this is very lenient

			var stgdatAge = DateTime.UtcNow.Subtract(failed.StgdatVersion.LastWriteTimeUtc);
			if (stgdatAge.TotalMinutes < timeoutMinutes)
			{
				cmndatArchiveRequests.Enqueue(failed); // re-enqueue, it might show up later
			}
			else
			{
				logger.Error("Giving up after {0} minutes. Failed to find CMNDAT matching {1}",
					timeoutMinutes, failed.StgdatVersion.FileInfo.FullName);
			}
		}
	}

	private static bool TryResolveRequest(CmndatArchiveRequest request, IEnumerable<KeyValuePair<FileVersion, byte[]>> capturedCmndats)
	{
		var stgdatDir = request.StgdatVersion.FileInfo.Directory!.FullName;

		(long, KeyValuePair<FileVersion, byte[]>) bestMatch = (TimeSpan.FromDays(1).Ticks, default);

		foreach (var kvp in capturedCmndats)
		{
			var cmndatVersion = kvp.Key;
			if (cmndatVersion.FileInfo.Directory!.FullName != stgdatDir)
			{
				continue; // it must come from the same dir
			}
			var span = cmndatVersion.LastWriteTimeUtc.Subtract(request.StgdatVersion.LastWriteTimeUtc);
			long ticks = Math.Abs(span.Ticks);
			if (ticks < bestMatch.Item1)
			{
				bestMatch = (ticks, kvp);
			}
		}

		if (TimeSpan.FromTicks(bestMatch.Item1).TotalSeconds > 15)
		{
			// Probably not the CMNDAT we are looking for
			return false;
		}

		var targetPath = Path.Combine(request.ArchiveDir.FullName, "CMNDAT.BIN");
		if (File.Exists(targetPath))
		{
			logger.Error("Did we already resolve this? {0}", targetPath);
			return true; // because there is no need to retry
		}
		else
		{
			File.WriteAllBytes(targetPath, bestMatch.Item2.Value);
			return true;
		}
	}

	private void ProcessInternalFile(FileSystemEventArgs e)
	{
		// TODO check if it is our config file and refresh it
	}

	private static void WriteInfoText(DirectoryInfo archiveDir, FileVersion stgdatVersion)
	{
		var now = DateTime.UtcNow;
		const string timeFormat = "yyyy-MM-dd HH:mm:ss.fff";

		string text = $@"Path: {stgdatVersion.FileInfo.FullName}
LastModified: {stgdatVersion.LastWriteTimeUtc.ToLocalTime().ToString(timeFormat)}
       * UTC: {stgdatVersion.LastWriteTimeUtc.ToString(timeFormat)}

LoadCompleted: {now.ToLocalTime().ToString(timeFormat)}
        * UTC: {now.ToString(timeFormat)}";

		File.WriteAllText(Path.Combine(archiveDir.FullName, "info.txt"), text);
	}
}