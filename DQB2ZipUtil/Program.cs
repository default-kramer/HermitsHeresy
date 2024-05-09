using System.IO.Compression;

// A STGDAT file contains a header that is not compressed followed by
// the rest of the content which is compressed.
// Thank you turtle-insect for figuring all this stuff out.
const int headerLength = 0x110;

CompressionMode mode;
switch (args.ElementAtOrDefault(0))
{
	case "-c":
	case "-compress":
		mode = System.IO.Compression.CompressionMode.Compress;
		break;
	case "-d":
	case "-decompress":
		mode = System.IO.Compression.CompressionMode.Decompress;
		break;
	default:
		return ShowUsage();
}

string? from = args.ElementAtOrDefault(1);
string? to = args.ElementAtOrDefault(2);
if (from == null || to == null)
{
	return ShowUsage();
}

var src = new System.IO.FileInfo(from);
if (!src.Exists)
{
	Console.WriteLine("File does not exist: {0}", src.FullName);
	return ErrorCode.SourceFileNotExists;
}
var dst = new FileInfo(to);

if (mode == CompressionMode.Compress)
{
	return Compress(src, dst);
}
else if (mode == CompressionMode.Decompress)
{
	return Decompress(src, dst);
}
else
{
	throw new Exception("Assert fail");
}

int ShowUsage()
{
	Console.WriteLine("Usage: DQB2ZipUtil <mode> <infile> <outfile>");
	Console.WriteLine("  <mode> must be one of: -c -compress -d -decompress");
	return ErrorCode.BadUsage;
}

int Compress(FileInfo src, FileInfo dst)
{
	using var inStream = new FileStream(src.FullName, FileMode.Open, FileAccess.Read);
	using var outStream = new FileStream(dst.FullName, FileMode.Create);

	// write header without compression
	var header = new byte[headerLength];
	inStream.ReadExactly(header, 0, headerLength);
	outStream.Write(header);

	// write remainder with compression
	// WARNING - CompressionLevel.Fastest occasionally causes DQB2 to misread the file, and
	// the southern half (approximately) of your IoA will be totally clear!
	// I have not seen this problem with CompressionLevel.Optimal, knock on wood...
	using var zlib = new ZLibStream(outStream, CompressionLevel.Optimal);
	inStream.CopyTo(zlib);

	outStream.Flush();
	return 0;
}

int Decompress(FileInfo src, FileInfo dst)
{
	using var inStream = new FileStream(src.FullName, FileMode.Open, FileAccess.Read);

	// validate header
	var header = new byte[headerLength];
	inStream.ReadExactly(header, 0, headerLength);
	Byte[] expected = { 0x61, 0x65, 0x72, 0x43 };
	for (int i = 0; i < expected.Length; i++)
	{
		if (expected[i] != header[i])
		{
			Console.WriteLine("Source file does not appear to be a DQB2 STGDAT file");
			return ErrorCode.NotSTGDAT;
		}
	}

	// write header without decompression
	using var outStream = new FileStream(dst.FullName, FileMode.Create);
	outStream.Write(header);

	// write remainder with decompression
	using var zlib = new ZLibStream(inStream, CompressionMode.Decompress);
	zlib.CopyTo(outStream);

	outStream.Flush();
	return 0;
}

static class ErrorCode
{
	public const int BadUsage = 1;
	public const int SourceFileNotExists = 2;
	public const int NotSTGDAT = 3;

	public const int AssertFail = 911;
}
