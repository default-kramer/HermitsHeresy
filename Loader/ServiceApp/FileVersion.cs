using System;
using System.Collections.Generic;
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
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

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

	private static (FileVersion, byte[]) DoReadBytes(string fullPath)
	{
		var writeTime = File.GetLastWriteTimeUtc(fullPath);
		var rawContent = File.ReadAllBytes(fullPath);
		var snap = new FileVersion(new FileInfo(fullPath), writeTime);
		return (snap, rawContent);
	}

	public static bool TryReadBytes(string fullPath, out FileVersion fileVersion, out byte[] bytes)
	{
		try
		{
			(fileVersion, bytes) = DoReadBytes(fullPath);
			return true;
		}
		catch (IOException ex)
		{
			// Probably DQB2 still has the file locked
			logger.Debug(ex, "TryReadBytes failed, file is locked probably? {0}", fullPath);
			fileVersion = default;
			bytes = null!;
			return false;
		}
	}

	public override string ToString()
	{
		return $"({LastWriteTimeUtc.ToString("yyyyMMdd-HHmmss")} :: {FullPath})";
	}
}
