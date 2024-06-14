create table SourceFile (
	SourceFileId int primary key
	, OriginalPath text not null
	, ModifiedDate text not null /* ISO8601 */
	, SlotName text null /* eg "user:B00:STGDAT01" or whatever */
	, ArchivePath text not null
);

create table Stage (
	StageId int primary key
	, ParentStageId int null references Stage(StageId) /* cloned from */
	, OriginalStageId int null references Stage(StageId) /* follows "cloned from" all the way back to the root */
);

create table Chunk (
	ChunkId int primary key
	, OffsetX int not null
	, OffsetZ int not null
	, BlocksHash text null /* blocks only, not items */
);

create table StageChunk (
	StageId int not null references Stage(StageId)
	, ChunkId int not null references Chunk(ChunkId)
	, primary key (StageId, ChunkId)
);

create table Cell (
	ChunkId int not null references Chunk(ChunkId)
	, X int not null
	, Z int not null
	, Y int not null
	, MaskedBlockVal int not null /* BlockVal & 0x7FF */
	, BlockVal int not null
);

create index idx_Cell_ChunkId on Cell(ChunkId);
create index idx_Cell_X on Cell(X);
create index idx_Cell_Z on Cell(Z);
create index idx_Cell_Y on Cell(Y);
create index idx_Cell_MaskedBlockVal on Cell(MaskedBlockVal);
