create table SourceFile (
	SourceFileId integer primary key
	, OriginalPath text not null
	, ModifiedDate text not null /* ISO8601 */
	, SlotName text null /* eg "user:B00:STGDAT01" or whatever */
	, ArchivePath text not null
);

create table Stage (
	StageId integer primary key
	/* , SourceFileId int null references SourceFile(SourceFileId) */
	, ParentStageId int null references Stage(StageId) /* cloned from */
	, OriginalStageId int null references Stage(StageId) /* follows "cloned from" all the way back to the root */
);

/*
Inserting block data appears to be one of the costliest operations (CPU cycles and disk space).
But when I'm building, typically only a few chunks change on each save.
So I want to be able to reuse Chunks that have not changed.
The BlocksHash will store the SHA1 of the 0x30000 STGDAT bytes.
When BlocksHash is not null, the Chunk (and its Cells) must be read-only/immutable;
this will be enforced in application logic.

Because the Chunk's Cells use world coordinates, we also have to
include the Chunk's offset when attempting to find an unchanged Chunk.
Thus the Offset belongs in this table (rather than in StageChunk).

I am assuming that a SHA1 collision is essentially impossible.
If this turns out not to be correct... it won't be fun.
*/
create table Chunk (
	ChunkId integer primary key
	, OffsetX integer not null
	, OffsetZ integer not null
	, BlocksHash text null
);

create table StageChunk (
	StageId integer not null references Stage(StageId)
	, ChunkId integer not null references Chunk(ChunkId)
	, primary key (StageId, ChunkId)
);

create table Cell (
	ChunkId integer not null references Chunk(ChunkId)
	, X integer not null
	, Z integer not null
	, Y integer not null
	, MaskedBlockVal integer not null /* BlockVal & 0x7FF */
	, BlockVal integer not null
);

create index idx_Cell_ChunkId on Cell(ChunkId);
create index idx_Cell_X on Cell(X);
create index idx_Cell_Z on Cell(Z);
create index idx_Cell_Y on Cell(Y);
create index idx_Cell_MaskedBlockVal on Cell(MaskedBlockVal);
