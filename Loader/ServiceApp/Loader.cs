using HH.Core;
using System.Data.SQLite;
using static HH.Core.Stgdat;

namespace ServiceApp;

sealed class Loader
{
	private readonly Stgdat stgdat;
	private readonly SQLiteConnection connection;

	public Loader(Stgdat stgdat, SQLiteConnection connection)
	{
		this.stgdat = stgdat;
		this.connection = connection;
	}

	public void Load()
	{
		if (!stgdat.ParseItems(out var items))
		{
			return;
		}

		var anchors = items.ToDictionary(x => x.Anchor);
		if (!stgdat.CompleteItems(anchors, out var completeItems))
		{
			return;
		}

		LoadItems(completeItems);

		var itemLookup = new Dictionary<Point, CompleteItem>();
		foreach (var item in completeItems)
		{
			foreach (var point in item.Extent)
			{
				itemLookup.Add(point, item);
			}
		}

		// Now we know the shared ID of the item in each cell... but is that even the data model I want to use?
		//LoadBlocks(itemLookup);
	}

	public void LoadBlocksOnly() => LoadBlocks();

	private void LoadItems(IReadOnlyList<CompleteItem> items)
	{
		var trx = connection.BeginTransaction();
		var command = connection.CreateCommand();
		command.CommandText = "insert into Item(SharedId, AnchorX, AnchorZ, AnchorY, BlockVal, ItemVal, Facing) values(?,?,?,?,?,?,?)";
		command.Transaction = trx;
		var pSharedId = command.AddParam();
		var pAnchorX = command.AddParam();
		var pAnchorZ = command.AddParam();
		var pAnchorY = command.AddParam();
		var pBlockVal = command.AddParam();
		var pItemVal = command.AddParam();
		var pFacing = command.AddParam();
		command.Prepare();

		foreach (var item in items)
		{
			var i = item.Item;
			pSharedId.Value = item.SharedId;
			pAnchorX.Value = i.Anchor.X;
			pAnchorZ.Value = i.Anchor.Z;
			pAnchorY.Value = i.Anchor.Y;
			pBlockVal.Value = i.BlockVal;
			pItemVal.Value = i.ItemVal;
			pFacing.Value = i.Facing;
			command.ExecuteNonQuery();
		}

		trx.Commit();
	}

	private void LoadBlocks()
	{
		var trx = connection.BeginTransaction();

		var command = connection.CreateCommand();
		command.CommandText = "insert into Cell(X,Z,Y,BlockVal) values(?,?,?,?)";
		command.Transaction = trx;
		var pX = command.AddParam();
		var pZ = command.AddParam();
		var pY = command.AddParam();
		var pBlockVal = command.AddParam();
		command.Prepare();

		for (int chunk = 0; chunk < stgdat.NumChunks; chunk++)
		{
			var buffer = stgdat.GetChunkBytes(chunk);
			int address = 0;
			var offset = stgdat.ChunkToOffset(chunk);

			for (int y = 0; y < 96; y++)
			{
				pY.Value = y;
				for (int z = offset.StartZ; z < offset.EndZ; z++)
				{
					pZ.Value = z;
					for (int x = offset.StartX; x < offset.EndX; x++)
					{
						byte lo = buffer[address];
						address++;
						byte hi = buffer[address];
						address++;
						int blockVal = lo | (hi << 8);

						if (blockVal != 0)
						{
							pX.Value = x;
							pBlockVal.Value = blockVal;
							command.ExecuteNonQuery();
						}
					}
				}
			}
		}

		trx.Commit();
	}
}

static class Util
{
	public static SQLiteParameter AddParam(this SQLiteCommand command)
	{
		var param = command.CreateParameter();
		command.Parameters.Add(param);
		return param;
	}
}
