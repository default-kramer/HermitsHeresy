using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp.Work;

/// <summary>
/// Copies a STGDAT file from a live directory to an archive directory.
/// </summary>
sealed class ArchiveStgdatWork : ReadFileWork
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

	private readonly CmndatManager cmndatManager;
	private readonly DirectoryInfo archiveRoot;

	public ArchiveStgdatWork(string fullPath, CmndatManager cmndatManager, DirectoryInfo archiveRoot) : base(fullPath)
	{
		this.cmndatManager = cmndatManager;
		this.archiveRoot = archiveRoot;
	}

	protected override void OnReadFileCompleted(WorkQueue queue, FileVersion stgdatVersion, byte[] stgdatBytes)
	{
		// Remember that WorkItem code is essentially single-threaded, so if we get a duplicate notification
		// for the same file we will see that the archive subdirectory already exists.
		string archiveName = stgdatVersion.LastWriteTimeUtc.ToString("yyyyMMdd-HHmmss")
			+ "-" + (stgdatVersion.FileInfo.Directory?.Name ?? "WTF")
			+ "-" + Path.GetFileNameWithoutExtension(stgdatVersion.FileInfo.Name);

		var archiveDir = new DirectoryInfo(Path.Combine(this.archiveRoot.FullName, archiveName));
		if (archiveDir.Exists)
		{
			// Hmm, it looks like this is expected behavior (getting multiple notifications for the same write)...
			// So far it seems to work fine, but how can I be sure I copied all the bytes I wanted?
			logger.Warn("Already archived? {0}", stgdatVersion);
			return;
		}
		else
		{
			archiveDir.Create();
		}

		var targetFile = Path.Combine(archiveDir.FullName, stgdatVersion.FileInfo.Name);
		File.WriteAllBytes(targetFile, stgdatBytes);
		WriteInfoText(archiveDir, stgdatVersion);
		logger.Info("Archived STGDAT file, {0} bytes written to {1}", stgdatBytes.Length, targetFile);

		cmndatManager.AddArchiveRequest(stgdatVersion, archiveDir);
		cmndatManager.ProcessArchiveRequests();

		// Probably need another worker thread for the SQL connection?
		/*
		var helper = helperFactory(stgdatContent);
		var stgdat = new Stgdat(helper);
		new Loader(stgdat, conn).Load();
		*/
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

		var filename = Path.Combine(archiveDir.FullName, "info.txt");
		logger.Info("Writing info to {0}", filename);
		// The file should not already exist, but if it does using AppendAllText could make debugging easier
		File.AppendAllText(filename, text);
	}
}
