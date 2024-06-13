using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HH.Core;

public abstract class StgdatHelper
{
	private readonly byte[] rawContent;
	private readonly IReadOnlyList<Offset> chunkMap; // chunk -> offset
	private readonly IReadOnlyList<IReadOnlyList<int>> offsetMap; // offset Z -> offset X -> chunk

	/// <param name="rawContent">The entire STGDAT file, uncompressed and including the header</param>
	public StgdatHelper(byte[] rawContent, IReadOnlyList<Offset> chunkMap)
	{
		this.rawContent = rawContent;
		this.chunkMap = chunkMap;

		var maxOX = chunkMap.Max(offset => offset.OX) + 1;
		var maxOZ = chunkMap.Max(offset => offset.OZ) + 1;

		// Initialize all values to -1 indicating "no chunk at this offset"
		var map = Enumerable.Range(0, maxOZ).Select(_ => Enumerable.Repeat(-1, maxOX).ToList()).ToList();
		this.offsetMap = map;

		for (int chunk = 0; chunk < chunkMap.Count; chunk++)
		{
			var offset = chunkMap[chunk];
			map[offset.OZ][offset.OX] = chunk;
		}
	}

	public int NumChunks => chunkMap.Count;

	public Offset ChunkToOffset(int chunk) => chunkMap[chunk];

	public int OffsetToChunk(Offset offset) => offsetMap[offset.OZ][offset.OX];

	public abstract Offset SillyOffsetMath(int whichChunk);

	internal (byte[] header, byte[] buffer) Decompress()
	{
		Byte[] check = { 0x61, 0x65, 0x72, 0x43, };
		for (int i = 0; i < check.Length; i++)
		{
			if (check[i] != rawContent[i]) throw new Exception("TODO");
		}

		var header = new Byte[0x110];
		var compressed = new Byte[rawContent.Length - header.Length];
		Array.Copy(rawContent, header.Length, compressed, 0, compressed.Length);
		Array.Copy(rawContent, header, header.Length);
		return (header, Decomp(compressed));
	}

	private static Byte[] Decomp(Byte[] data)
	{
		using var input = new MemoryStream(data);
		using var zlib = new System.IO.Compression.ZLibStream(input, System.IO.Compression.CompressionMode.Decompress);
		using var output = new MemoryStream();
		zlib.CopyTo(output);
		output.Flush();
		zlib.Flush();
		return output.ToArray();
	}

	public class IoA : StgdatHelper
	{
		public IoA(byte[] rawContent) : base(rawContent, Util.OffsetsIoA) { }

		public override Offset SillyOffsetMath(int whichChunk)
		{
			return new Offset((whichChunk % 64) - 18, (whichChunk / 64) - 23);
		}
	}
}
