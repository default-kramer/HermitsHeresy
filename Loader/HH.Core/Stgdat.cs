using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HH.Core;

public sealed class Stgdat
{
	private readonly StgdatHelper helper;
	private readonly byte[] header;
	private readonly byte[] buffer;
	private Lazy<(bool, IReadOnlyList<Item>)> parseItems;

	public Stgdat(StgdatHelper request)
	{
		this.helper = request;
		(this.header, this.buffer) = request.Decompress();
		this.parseItems = new Lazy<(bool, IReadOnlyList<Item>)>(ParseItems);
	}

	public bool IsFragmented => !ParseItems(out _);

	public int NumChunks => helper.NumChunks;

	public ReadOnlySpan<byte> GetChunkBytes(int chunk)
	{
		var addr = BlockAddress(chunk);
		return buffer.AsSpan().Slice(addr, 0x30000);
	}

	public Offset ChunkToOffset(int chunk) => helper.ChunkToOffset(chunk);

	public record struct Item(Point Anchor, int BlockVal, int ItemVal, int Facing, int DefragIndex);

	// DAMN this won't work! Because two items might be back-to-back and you wouldn't be
	// able to tell the difference between (1x2x1, 1x2x1) vs (1x1x1, 1x3x1)
	// SO WHAT DO I DO?
	// AHA - As long as you don't find another anchor within the flooded area, then you
	// are safe, so maybe we can gradually learn the item dimensions...
	// But how to save?
	// Maybe if we definitely know the dimensions, we can save it.
	// Otherwise we leave the spot blank.
	// NO - For now, create custom files that have no adjacency, duh!
	public record struct CompleteItem(Item Item, IReadOnlySet<Point> Extent, int SharedId);

	public bool ParseItems(out IReadOnlyList<Item> items)
	{
		var result = parseItems.Value;
		items = result.Item2;
		return result.Item1;
	}

	private int ItemCount
	{
		get { return ReadNumber(0x24E7CD, 3); }
		set { WriteNumber(0x24E7CD, 3, value); }
	}

	private void SetDefragIndex(int i, int defragIndex)
	{
		int addr4 = 0x150E7D1 + 4 * i;
		int temp = ReadNumber(addr4 + 1, 1);
		temp = temp & 0xF0 | (defragIndex & 0x0F);
		WriteNumber(addr4 + 1, 1, temp);
		WriteNumber(addr4 + 2, 2, defragIndex >> 4);
	}

	private Item ReadItem(int i)
	{
		int addr24 = 0x24E7D1 + 24 * i;
		int addr4 = 0x150E7D1 + 4 * i;

		int itemVal = ReadNumber(addr24 + 8, 2) & 0x1FFF;
		int dx = ReadNumber(addr24 + 9, 1) >> 5;

		int temp = ReadNumber(addr24 + 10, 1);
		dx |= (temp & 0b0000_0011) << 3;
		int y = temp >> 2;

		temp = ReadNumber(addr24 + 11, 1);
		y |= (temp & 1) << 6;
		int dz = (temp & 0b0011_1110) >> 1;
		int facing = (temp & 0b1100_0000) >> 6;

		int whichChunk = ReadNumber(addr4, 2) & 0xFFF;
		var offset = helper.SillyOffsetMath(whichChunk);

		temp = buffer[addr4 + 1];
		int defragIndex = ((temp & 0xF0) >> 4) | (ReadNumber(addr4 + 2, 2) << 4);

		var anchor = new Point(offset.StartX + dx, offset.StartZ + dz, y);
		var blockVal = ReadBlock(anchor);
		return new Item()
		{
			Anchor = anchor,
			BlockVal = blockVal,
			ItemVal = itemVal,
			Facing = facing,
			DefragIndex = defragIndex,
		};
	}

	private (bool, IReadOnlyList<Item>) ParseItems()
	{
		int itemCount = ItemCount;
		if (itemCount >= 0xC8000)
		{
			throw new Exception("TODO");
		}

		var items = new List<Item>();

		// The 0th item is special, skip it
		for (int i = 1; i < itemCount; i++)
		{
			var item = ReadItem(i);
			if (item.DefragIndex != i)
			{
				// This file is fragmented and I don't know how to parse the items.
				// Using DQB2 to load and immediately save sometimes defragments the file.
				return (false, items);
			}
			items.Add(item);
		}

		return (true, items);
	}

	/// <summary>
	/// Keep expanding the extent as long as we find the same blockVal.
	/// This method will only work if the item is not adjacent to another item.
	/// If adjacency is detected, we return false.
	/// (For adjacent items, we will need the dimensions to know which is which.)
	/// </summary>
	private bool TryGetExtent(Item item, IReadOnlyDictionary<Point, Item> anchors, out HashSet<Point> extent)
	{
		extent = new HashSet<Point>() { item.Anchor };
		var neighbors = item.Anchor.ExtentHelper().ToHashSet();

		while (neighbors.Count > 0)
		{
			var nextCycle = new List<Point>();

			foreach (var point in neighbors)
			{
				if (ReadBlock(point) == item.BlockVal)
				{
					if (anchors.ContainsKey(point))
					{
						return false; // Adjacency detected
					}
					extent.Add(point);
					nextCycle.AddRange(point.ExtentHelper());
				}
			}

			neighbors = nextCycle.ToHashSet();
			neighbors.RemoveWhere(extent.Contains);
		}

		return true;
	}

	public bool CompleteItems(IReadOnlyDictionary<Point, Item> anchors, out List<CompleteItem> completeItems)
	{
		completeItems = new List<CompleteItem>();
		int sharedId = 1;

		foreach (var item in anchors.Values)
		{
			if (TryGetExtent(item, anchors, out var extent))
			{
				completeItems.Add(new CompleteItem(item, extent, sharedId));
				sharedId++;
			}
			else
			{
				return false;
			}
		}

		return true;
	}

	private int BlockAddress(int chunk, int y = 0, int dz = 0, int dx = 0)
	{
		int offset = chunk * 96 * 32 * 32
			+ y * 32 * 32
			+ dz * 32
			+ dx;

		return 0x183FEF0 + 2 * offset;
	}

	private int BlockAddress(Point point)
	{
		var off = point.XZ.AsOffset();
		var chunk = helper.OffsetToChunk(off.offset);
		return BlockAddress(chunk, y: point.Y, dz: off.payload.Z, dx: off.payload.X);
	}

	private int ReadNumber(int address, int size)
	{
		if (size > 3)
		{
			throw new Exception("TODO switch to uint?");
		}

		int result = 0;
		for (int i = 0; i < size; i++)
		{
			int val = buffer[address + i];
			result += val << (i * 8);
		}
		return result;
	}

	private void WriteNumber(int address, int size, int value)
	{
		for (int i = 0; i < size; i++)
		{
			buffer[address + i] = (Byte)(value & 0xFF);
			value >>= 8;
		}
	}

	public int ReadBlock(Point point)
	{
		var address = BlockAddress(point);
		return ReadNumber(address, 2);
	}

	public void ClearBlock(Point point)
	{
		var address = BlockAddress(point);
		buffer[address] = 0;
		buffer[address + 1] = 0;
	}

	private void ClearItem(Item deleteItem)
	{
		var lastIdx = ItemCount - 1;
		SetDefragIndex(deleteItem.DefragIndex, lastIdx);
		SetDefragIndex(lastIdx, deleteItem.DefragIndex);
		ItemCount = lastIdx;
		// When DQB2 starts up, I expect it to skip over the deleteItem because its DefragIndex >= ItemCount.
	}

	public IEnumerable<(Item Item, Point Dimensions, IReadOnlySet<Point> Extent)> ComputeItemDimensions()
	{
		if (!ParseItems(out var items))
		{
			yield break;
		}

		var anchors = items.ToDictionary(i => i.Anchor);
		foreach (var item in items)
		{
			if (TryGetExtent(item, anchors, out var extent))
			{
				var minX = extent.Min(p => p.X);
				var maxX = extent.Max(p => p.X);
				var minZ = extent.Min(p => p.Z);
				var maxZ = extent.Max(p => p.Z);
				var minY = extent.Min(p => p.Y);
				var maxY = extent.Max(p => p.Y);
				var width = 1 + maxX - minX;
				var depth = 1 + maxZ - minZ;
				var height = 1 + maxY - minY;

				switch (item.Facing)
				{
					case 0:
					case 2:
						yield return (item, new Point(width, depth, height), extent);
						break;
					case 1:
					case 3:
						yield return (item, new Point(depth, width, height), extent);
						break;
				}
			}
		}
	}

	public void RemoveItem(Item item, IReadOnlySet<Point> extent)
	{
		foreach (var point in extent)
		{
			if (ReadBlock(point) != item.BlockVal)
			{
				throw new Exception("TODO");
			}
			ClearBlock(point);
		}
		ClearItem(item);
	}

	public Byte[] Export()
	{
		using var output = new MemoryStream();
		using var header = new MemoryStream(this.header);
		header.CopyTo(output);

		using var body = new MemoryStream(this.buffer);
		using var zlib = new System.IO.Compression.ZLibStream(output, System.IO.Compression.CompressionLevel.Optimal);
		body.CopyTo(zlib);
		zlib.Flush();

		return output.ToArray();
	}
}