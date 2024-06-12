create table Cell (
	X int not null
	, Z int not null
	, Y int not null
	, BlockVal int not null
);

create index idx_Cell_X on Cell(X);
create index idx_Cell_Z on Cell(Z);
create index idx_Cell_Y on Cell(Y);
create index idx_Cell_BlockVal on Cell(BlockVal);
