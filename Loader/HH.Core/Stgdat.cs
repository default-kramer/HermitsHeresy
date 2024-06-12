using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HH.Core;

public sealed class Stgdat
{
	private readonly StgdatHelper helper;
	private readonly byte[] buffer;

	public Stgdat(StgdatHelper request)
	{
		this.helper = request;
		this.buffer = request.Decompress();
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

	public bool ParseItems(out List<Item> items)
	{
		int itemCount = ReadNumber(0x24E7CD, 3);
		if (itemCount >= 0xC8000)
		{
			throw new Exception("TODO");
		}

		items = new List<Item>();

		// The 0th item is special, skip it
		for (int i = 1; i < itemCount; i++)
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
			if (defragIndex != i)
			{
				// This file is fragmented and I don't know how to parse the items.
				// Using DQB2 to load and immediately save sometimes defragments the file.
				return false;
			}

			var anchor = new Point(offset.StartX + dx, offset.StartZ + dz, y);
			var blockVal = ReadBlock(anchor);
			items.Add(new Item()
			{
				Anchor = anchor,
				BlockVal = blockVal,
				ItemVal = itemVal,
				Facing = facing,
				DefragIndex = defragIndex,
			});
		}

		return true;
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

	public int ReadBlock(Point point)
	{
		var off = point.XZ.AsOffset();
		var chunk = helper.OffsetToChunk(off.offset);
		var address = BlockAddress(chunk, y: point.Y, dz: off.payload.Z, dx: off.payload.X);
		return ReadNumber(address, 2);
	}
}