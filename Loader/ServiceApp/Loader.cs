using HH.Core;
using System.Data;
using System.Data.SQLite;
using static HH.Core.Stgdat;

namespace ServiceApp;

public sealed class Loader
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

	private readonly Stgdat stgdat;
	private readonly SQLiteConnection connection;

	public Loader(Stgdat stgdat, SQLiteConnection connection)
	{
		this.stgdat = stgdat;
		this.connection = connection;
	}

	public int NewChunks { get; private set; }
	public int ReusedChunks { get; private set; }

	private bool LoadItems(out Dictionary<Point, CompleteItem> itemLookup)
	{
		if (!stgdat.ParseItems(out var items))
		{
			itemLookup = null!;
			return false;
		}

		var anchors = items.ToDictionary(x => x.Anchor);
		if (!stgdat.CompleteItems(anchors, out var completeItems))
		{
			itemLookup = null!;
			return false;
		}

		DoLoadItems(completeItems);

		itemLookup = new Dictionary<Point, CompleteItem>();
		foreach (var item in completeItems)
		{
			foreach (var point in item.Extent)
			{
				itemLookup.Add(point, item);
			}
		}

		return true;
	}

	private void DoLoadItems(IReadOnlyList<CompleteItem> items)
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

	public void LoadBlocks(long StageId)
	{
		int countNew = 0;
		int countReused = 0;

		var startTime = DateTime.UtcNow;
		var trx = connection.BeginTransaction();

		foreach (var chunk in stgdat.IterateChunks)
		{
			long ChunkId;
			var sha1 = stgdat.GetChunkSHA1(chunk);
			var offset = stgdat.ChunkToOffset(chunk);

			if (TryFindChunkToReuse(sha1, offset, out ChunkId))
			{
				InsertStageChunkMapping(StageId, ChunkId);
				countReused++;
			}
			else
			{
				ChunkId = InsertChunk(sha1, offset);
				InsertStageChunkMapping(StageId, ChunkId);
				InsertCells(chunk, ChunkId);
				countNew++;
			}
		}

		trx.Commit();
		var elapsedTime = DateTime.UtcNow.Subtract(startTime);

		logger.Info("Loaded {0} new chunks and {1} reused chunks to StageId {2} in {3} seconds",
			countNew, countReused, StageId, elapsedTime.TotalSeconds.ToString("0.00"));

		this.NewChunks += countNew;
		this.ReusedChunks += countReused;
	}

	private bool TryFindChunkToReuse(string sha1, Offset offset, out long ChunkId)
	{
		using var command = connection.CreateCommand();
		command.CommandText = "select ChunkId from Chunk where BlocksHash=@hash and OffsetX=@ox and OffsetZ=@oz";
		command.Parameters.AddWithValue("hash", sha1);
		command.Parameters.AddWithValue("ox", offset.OX);
		command.Parameters.AddWithValue("oz", offset.OZ);
		object result = command.ExecuteScalar();
		if (result == null || result == DBNull.Value)
		{
			ChunkId = default;
			return false;
		}

		ChunkId = Convert.ToInt32(result);
		return true;
	}

	private long InsertChunk(string sha1, Offset offset)
	{
		using var command = connection.CreateCommand();
		command.CommandText = "insert into Chunk(BlocksHash,OffsetX,OffsetZ) values(@hash,@ox,@oz); select last_insert_rowid();";
		command.Parameters.AddWithValue("hash", sha1);
		command.Parameters.AddWithValue("ox", offset.OX);
		command.Parameters.AddWithValue("oz", offset.OZ);
		object result = command.ExecuteScalar();
		long ChunkId = Convert.ToInt64(result);
		return ChunkId;
	}

	private void InsertStageChunkMapping(long StageId, long ChunkId)
	{
		using var command = connection.CreateCommand();
		command.CommandText = "insert into StageChunk(StageId,ChunkId) values(@stageId,@chunkId)";
		command.Parameters.AddWithValue("stageId", StageId);
		command.Parameters.AddWithValue("chunkId", ChunkId);
		command.ExecuteNonQuery();
	}

	private void InsertCells(int chunk, long ChunkId)
	{
		using var command = connection.CreateCommand();
		command.CommandText = "insert into Cell(ChunkId,X,Z,Y,MaskedBlockVal,BlockVal) values(@chunkId,@x,@z,@y,@maskedBlockVal,@blockVal)";
		command.Parameters.AddWithValue("chunkId", ChunkId);
		var pX = command.Parameters.Add("x", DbType.Int32);
		var pZ = command.Parameters.Add("z", DbType.Int32);
		var pY = command.Parameters.Add("y", DbType.Int32);
		var pMaskedBlockVal = command.Parameters.Add("maskedBlockVal", DbType.Int32);
		var pBlockVal = command.Parameters.Add("blockVal", DbType.Int32);
		command.Prepare();

		var offset = stgdat.ChunkToOffset(chunk);
		var buffer = stgdat.GetChunkBytes(chunk);
		int address = 0;

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
						pMaskedBlockVal.Value = blockVal & 0x7FF;
						pBlockVal.Value = blockVal;
						command.ExecuteNonQuery();
					}
				}
			}
		}
	}
}
