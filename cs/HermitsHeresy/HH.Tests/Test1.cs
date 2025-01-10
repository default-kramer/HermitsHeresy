using HH.Core;
using System.Diagnostics.CodeAnalysis;
using System.Drawing;
using System.Security.Cryptography;

namespace HH.Tests
{
	[TestClass]
	public sealed class Test1
	{
		[TestMethod]
		public void blah()
		{
			var hill1 = Hill.CreateHill(new Rect(100, 1, 100, 1), xz => 50);
			Assert.AreEqual(@"
 x".ReplaceLineEndings(), hill1.PrintHill());

			Shell.TryCreate(hill1, out var shell1);
			var hill2 = shell1.CreateNewHill(new Shell.HillOptions() { CornerExtensionLimit = 99 });
			Assert.AreEqual(@"
   x  
 x h x
   x  ".ReplaceLineEndings(), hill2.PrintHill());

			Shell.TryCreate(hill2, out var shell2);
			var hill3 = shell2.CreateNewHill(new Shell.HillOptions() { CornerExtensionLimit = 99 });
			Assert.AreEqual(@"
     x    
   x h x  
 x h h h x
   x h x  
     x    ".ReplaceLineEndings(), hill3.PrintHill());
		}

		[TestMethod]
		public void TestMethod1()
		{
			const int offset = 100;
			const int width = 20;
			var bounds = new Rect(offset, width, offset, width);

			int? GetElevation(XZ xz)
			{
				int fx = Math.Abs(xz.X - (offset + width / 2));
				int fz = Math.Abs(xz.Z - (offset + width / 2));
				if (fx + fz <= 10)
				{
					return 37;
				}
				return null;
			}
			var hill = Hill.CreateHill(bounds, GetElevation);
			Assert.AreEqual(219, hill.Count);
			Assert.AreEqual(37, hill.MinElevation);

			if (!Shell.TryCreate(hill, out var shell1))
			{
				Assert.Fail("failed to create shell");
				return;
			}
			var hill2 = shell1.CreateNewHill(new Shell.HillOptions());
			Assert.AreEqual(279, hill2.Count);
			Assert.AreEqual(35, hill2.MinElevation);

			if (!Shell.TryCreate(hill2, out var shell2))
			{
				Assert.Fail("failed to create shell 2");
				return;
			}
			var hill3 = shell2.CreateNewHill(new Shell.HillOptions());
			Assert.AreEqual(353, hill3.Count);
			Assert.AreEqual(33, hill3.MinElevation);

			if (!Shell.TryCreate(hill3, out var shell3))
			{
				Assert.Fail("failed to create shell 3");
			}
			var hill4 = shell3.CreateNewHill(new Shell.HillOptions());
			//Assert.AreEqual(432, hill4.Count);
			Assert.AreEqual(31, hill4.MinElevation);

			var file = new FileInfo(@"C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD\B00\STGDAT12.BIN");
			if (!Stgdat.TryLoad(file, out var stgdat))
			{
				Assert.Fail("failed to load stgdat");
				return;
			}


			foreach (var xz in bounds.Expand(4).Iterate())
			{
				for (int y = 1; y < 96; y++)
				{
					var cell = new Cell(xz, y);

					if (hill2.InHill(cell))
					{
						// 3 grass
						stgdat.PutBlock(cell, 156); // spoiled soil
					}
					else if (hill3.InHill(cell))
					{
						stgdat.PutBlock(cell, 149); // chert
					}
					else if (hill4.InHill(cell))
					{
						stgdat.PutBlock(cell, 131); // dark dolomite
					}
				}
			}


			for (int z = 100; z < 200; z++)
			{
				for (int x = 100; x < 200; x++)
				{
					//stgdat.PutBlock(new Cell(new XZ(x, z), 50), 0);
				}
			}

			if (true)
			{
				using var stream = File.OpenWrite(file.FullName);
				stream.Write(stgdat.Export());
				stream.Flush();
				stream.Close();
			}
		}

		sealed class Stgdat
		{
			private readonly byte[] header;
			private readonly byte[] buffer;
			public FileInfo LoadedFrom { get; }
			private readonly ChunkGrid chunkGrid;

			public Stgdat(byte[] header, byte[] buffer, FileInfo loadedFrom, ChunkGrid chunkGrid)
			{
				this.header = header;
				this.buffer = buffer;
				this.LoadedFrom = loadedFrom;
				this.chunkGrid = chunkGrid;
			}

			public static bool TryLoad(FileInfo file, [MaybeNullWhen(false)] out Stgdat stgdat)
			{
				var rawContent = System.IO.File.ReadAllBytes(file.FullName);

				Byte[] check = { 0x61, 0x65, 0x72, 0x43, };
				for (int i = 0; i < check.Length; i++)
				{
					if (check[i] != rawContent[i])
					{
						stgdat = null;
						return false;
					}
				}

				var header = new Byte[0x110];
				var compressed = new Byte[rawContent.Length - header.Length];
				Array.Copy(rawContent, header.Length, compressed, 0, compressed.Length);
				Array.Copy(rawContent, header, header.Length);
				var buffer = Decomp(compressed);

				if (!ChunkGrid.TryReadChunkGrid(buffer, out var chunkGrid))
				{
					stgdat = null;
					return false;
				}

				stgdat = new Stgdat(header, buffer, file, chunkGrid);
				return true;
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

			public Byte[] Export()
			{
				using var output = new MemoryStream();
				using var header = new MemoryStream(this.header);
				header.CopyTo(output);

				using var body = new MemoryStream(this.buffer);
				using var zlib = new System.IO.Compression.ZLibStream(output, System.IO.Compression.CompressionLevel.SmallestSize);
				body.CopyTo(zlib);
				zlib.Flush();

				return output.ToArray();
			}

			public void PutBlock(Cell cell, int block)
			{
				var off = cell.XZ.AsOffset();
				if (chunkGrid.TryGetChunkId(off.offset, out int chunkId))
				{
					var xz = off.payload;

					int addr = 0x183FEF0;
					addr += chunkId * 0x30000;
					addr += cell.Y * 32 * 32 * 2;
					addr += xz.Z * 32 * 2;
					addr += xz.X * 2;

					int val = buffer.AsSpan().ReadNumber(addr, 2);
					if (IsSimple(val))
					{
						buffer.AsSpan().WriteNumber(addr, 2, block);
					}
				}
			}

			private static bool IsSimple(int block)
			{
				return (block & 0x7FF) < 1158;
			}
		}
	}
}
