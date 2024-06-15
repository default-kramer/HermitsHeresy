using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

/// <summary>
/// Assumes there will be a singleton instance per SD directory.
/// Assumes single-threaded execution model via <see cref="WorkQueue"/>.
/// </summary>
class CmndatManager
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

	/// <summary>
	/// The goal is to find the CMNDAT that belongs with the STGDAT and archive it in the same directory.
	/// </summary>
	/// <param name="StgdatVersion">A STGDAT file that has been archived</param>
	/// <param name="ArchiveDir">The archive directory</param>
	record struct CmndatArchiveRequest(FileVersion StgdatVersion, DirectoryInfo ArchiveDir);

	private readonly Queue<CmndatArchiveRequest> archiveRequests = new();

	// Key is the version of the CMNDAT file.
	// A null value indicates ambiguity, which should never happen in normal operation.
	// If we ever set any key to null, it should stay that way forever.
	private readonly Dictionary<FileVersion, byte[]?> cmndatCaptures = new();

	public void AddArchiveRequest(FileVersion originalStgdat, DirectoryInfo archiveDir)
	{
		archiveRequests.Enqueue(new CmndatArchiveRequest(originalStgdat, archiveDir));
	}

	public void CaptureCmndat(FileVersion cmndatVersion, byte[] cmndatBytes)
	{
		if (cmndatCaptures.TryGetValue(cmndatVersion, out var existingBytes))
		{
			if (existingBytes == null)
			{
				return; // already known to be ambiguous
			}
			else if (!cmndatBytes.SequenceEqual(existingBytes))
			{
				logger.Error("Different CMNDAT bytes exist for the same version! {0}", cmndatVersion);
				cmndatCaptures[cmndatVersion] = null;
			}
		}
		else
		{
			cmndatCaptures.Add(cmndatVersion, cmndatBytes);
		}
	}

	public void ProcessArchiveRequests()
	{
		List<CmndatArchiveRequest> unresolvedRequests = new();

		while (archiveRequests.TryDequeue(out var request))
		{
			if (!TryResolveRequest(request))
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
				archiveRequests.Enqueue(failed); // re-enqueue, it might show up later
			}
			else
			{
				logger.Error("Giving up after {0} minutes. Failed to find CMNDAT matching {1}",
					timeoutMinutes, failed.StgdatVersion.FileInfo.FullName);
			}
		}
	}

	private bool TryResolveRequest(CmndatArchiveRequest request)
	{
		var stgdatDir = request.StgdatVersion.FileInfo.Directory!.FullName;

		(long ticks, FileVersion, byte[])? bestMatch = null;

		foreach (var kvp in cmndatCaptures)
		{
			if (kvp.Value == null)
			{
				continue; // ambiguous, can't do anything
			}

			var cmndatVersion = kvp.Key;
			if (cmndatVersion.FileInfo.Directory!.FullName != stgdatDir)
			{
				continue; // it must come from the same dir
			}
			var span = cmndatVersion.LastWriteTimeUtc.Subtract(request.StgdatVersion.LastWriteTimeUtc);
			long ticks = Math.Abs(span.Ticks);
			if (bestMatch == null || ticks < bestMatch.Value.ticks)
			{
				bestMatch = (ticks, kvp.Key, kvp.Value);
			}
		}

		if (bestMatch == null || TimeSpan.FromTicks(bestMatch.Value.ticks).TotalSeconds > 15)
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
			var sourceVersion = bestMatch.Value.Item2;
			logger.Info("Archiving CMNDAT from {0} to {1}", sourceVersion.FileInfo.FullName, targetPath);
			File.WriteAllBytes(targetPath, bestMatch.Value.Item3);
			return true;
		}
	}
}
