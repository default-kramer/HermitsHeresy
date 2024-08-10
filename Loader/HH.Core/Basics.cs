namespace HH.Core;

public record struct Offset(int OX, int OZ)
{
	public int StartX => OX * 32;
	public int StartZ => OZ * 32;

	public int EndX => StartX + 32;
	public int EndZ => StartZ + 32;
}

public record struct Offset<T>(Offset offset, T payload);

public record struct XZ(int X, int Z)
{
	public Offset<XZ> AsOffset()
	{
		var ox = this.X / 32;
		var oz = this.Z / 32;
		var dx = this.X % 32;
		var dz = this.Z % 32;
		return new Offset<XZ>(new Offset(ox, oz), new XZ(dx, dz));
	}
}

public record struct Point(XZ XZ, int Y)
{
	public Point(int x, int z, int y) : this(new XZ(x, z), y) { }

	public int X => XZ.X;
	public int Z => XZ.Z;

	public Point Add(Point other)
	{
		return new Point(new XZ(this.X + other.X, this.Z + other.Z), this.Y + other.Y);
	}

	public IEnumerable<Point> ExtentHelper()
	{
		yield return this.Add(new Point(1, 0, 0));
		yield return this.Add(new Point(-1, 0, 0));
		yield return this.Add(new Point(0, 1, 0));
		yield return this.Add(new Point(0, -1, 0));
		yield return this.Add(new Point(0, 0, 1));
		// items never go down from the anchor, so y-1 is not needed
	}
}
