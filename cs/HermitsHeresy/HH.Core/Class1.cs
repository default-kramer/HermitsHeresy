using System.Collections;
using System.ComponentModel;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Text;

namespace HH.Core;

public static class Util
{
	/// <summary>
	/// Retypes the given <paramref name="value"/> as a Nullable.
	/// (Because I don't like seeing casts in my code.)
	/// </summary>
	public static T? AsNullable<T>(this T value) where T : struct
	{
		T? retval = value;
		return retval;
	}

	public static int ReadNumber(this Span<byte> buffer, int address, int size)
	{
		ReadOnlySpan<byte> ro = buffer;
		return ro.ReadNumber(address, size);
	}

	public static int ReadNumber(this ReadOnlySpan<byte> buffer, int address, int size)
	{
		int result = 0;
		for (int i = 0; i < size; i++)
		{
			result += buffer[address + i] << (i * 8);
		}
		return result;
	}

	public static void WriteNumber(this Span<byte> buffer, int address, int size, int value)
	{
		for (int i = 0; i < size; i++)
		{
			buffer[address + i] = (byte)(value & 0xFF);
			value >>= 8;
		}
	}
}

public record struct Offset(int OX, int OZ)
{
	public int StartX => OX * 32;
	public int StartZ => OZ * 32;

	public int EndX => StartX + 32;
	public int EndZ => StartZ + 32;
}

public record struct Offset<T>(Offset offset, T payload);

public readonly record struct XZ(int X, int Z)
{
	public Offset<XZ> AsOffset()
	{
		var ox = this.X / 32;
		var oz = this.Z / 32;
		var dx = this.X % 32;
		var dz = this.Z % 32;
		return new Offset<XZ>(new Offset(ox, oz), new XZ(dx, dz));
	}

	public IEnumerable<XZ> Neighbors()
	{
		yield return new XZ(X, Z - 1);
		yield return new XZ(X - 1, Z);
		yield return new XZ(X + 1, Z);
		yield return new XZ(X, Z + 1);
	}

	public IEnumerable<XZ> DiagonalNeighbors()
	{
		yield return new XZ(X - 1, Z - 1);
		yield return new XZ(X + 1, Z - 1);
		yield return new XZ(X - 1, Z + 1);
		yield return new XZ(X + 1, Z + 1);
	}

	public IEnumerable<XZ> NorthSouth()
	{
		yield return new XZ(X, Z - 1);
		yield return new XZ(X, Z + 1);
	}

	public IEnumerable<XZ> WestEast()
	{
		yield return new XZ(X - 1, Z);
		yield return new XZ(X + 1, Z);
	}
}

public readonly record struct Cell(XZ XZ, int Y);

public readonly record struct Rect(int StartX, int Width, int StartZ, int Height)
{
	public static readonly Rect Empty = new Rect();

	public int EndX => StartX + Width;
	public int EndZ => StartZ + Height;

	public IEnumerable<XZ> Iterate()
	{
		for (int z = StartZ; z < EndZ; z++)
		{
			for (int x = StartX; x < EndZ; x++)
			{
				yield return new XZ(x, z);
			}
		}
	}

	public bool Contains(XZ xz)
	{
		return xz.X >= StartX
			&& xz.Z >= StartZ
			&& xz.X < EndX
			&& xz.Z < EndZ;
	}

	/// <summary>
	/// Expands all 4 sides by the given <paramref name="amount"/>.
	/// </summary>
	public Rect Expand(int amount)
	{
		int widener = amount * 2;
		return new Rect(StartX - amount, Width + widener, StartZ - amount, Height + widener);
	}
}

public interface ILookup2D<T>
{
	Rect Bounds { get; }

	public bool TryGetValue(XZ key, [MaybeNullWhen(false)] out T value);

	int Count { get; }
}

sealed class MutableLookup2D<T>
{
	sealed class FinalizedLookup : ILookup2D<T>
	{
		public Rect Bounds { get; }
		private readonly IReadOnlyDictionary<XZ, T> dict;
		public int Count => dict.Count;

		public FinalizedLookup(Rect bounds, IReadOnlyDictionary<XZ, T> dict)
		{
			this.Bounds = bounds;
			this.dict = dict;
		}

		public bool TryGetValue(XZ key, [MaybeNullWhen(false)] out T value) => dict.TryGetValue(key, out value);
	}

	private IDictionary<XZ, T> dict;
	private int xMin = int.MaxValue;
	private int xMax = int.MinValue;
	private int zMin = int.MaxValue;
	private int zMax = int.MinValue;

	private MutableLookup2D()
	{
		this.dict = new Dictionary<XZ, T>();
	}

	/// <summary>
	/// Require caller to pass <paramref name="bounds"/> to allow us to switch to an array-backed
	/// implementation if benchmarks warrant doing so.
	/// </summary>
	public static MutableLookup2D<T> Create(Rect bounds)
	{
		return new MutableLookup2D<T>();
	}

	public bool TryGetValue(XZ key, [MaybeNullWhen(false)] out T value) => dict.TryGetValue(key, out value);

	public void Add(XZ xz, T value)
	{
		dict.Add(xz, value);
		xMin = Math.Min(xMin, xz.X);
		xMax = Math.Max(xMax, xz.X);
		zMin = Math.Min(zMin, xz.Z);
		zMax = Math.Max(zMax, xz.Z);
	}

	/// <summary>
	/// Shrinks the <see cref="Bounds"/> as far as possible and returns a readonly instance.
	/// </summary>
	public ILookup2D<T> Finish()
	{
		var originalDict = dict as Dictionary<XZ, T>;
		if (originalDict == null)
		{
			throw new InvalidOperationException("Already finished");
		}

		// Cause an exception if future mutations are requested:
		this.dict = dict.AsReadOnly();

		if (dict.Count == 0)
		{
			return new FinalizedLookup(Rect.Empty, originalDict);
		}
		else
		{
			var bounds = new Rect(xMin, 1 + xMax - xMin, zMin, 1 + zMax - zMin);
			return new FinalizedLookup(bounds, originalDict);
		}
	}
}

public sealed class Hill
{
	internal readonly record struct HillEntry(int elevation, bool exterior);

	private readonly ILookup2D<HillEntry> lookup;
	public int MinElevation { get; }
	public int Count => lookup.Count;

	private Hill(ILookup2D<HillEntry> lookup, int minElevation)
	{
		this.lookup = lookup;
		this.MinElevation = minElevation;
	}

	public bool InHill(Cell cell)
	{
		return lookup.TryGetValue(cell.XZ, out var entry)
			&& cell.Y < entry.elevation;
	}

	public static Hill CreateHill(Rect bounds, Func<XZ, int?> getElevation)
	{
		var lookup = MutableLookup2D<HillEntry>.Create(bounds);
		int minElevation = int.MaxValue;

		bool isEmpty(XZ xz) => !bounds.Contains(xz) || !getElevation(xz).HasValue;

		foreach (var xz in bounds.Iterate())
		{
			var elevation = getElevation(xz);
			if (elevation.HasValue)
			{
				if (elevation.Value > 0)
				{
					minElevation = Math.Min(elevation.Value, minElevation);
				}
				bool exterior = xz.Neighbors().Any(isEmpty);
				lookup.Add(xz, new HillEntry(elevation.Value, exterior));
			}
		}

		return new Hill(lookup.Finish(), minElevation);
	}

	internal void ShellHelper(out ILookup2D<HillEntry> lookup)
	{
		lookup = this.lookup;
	}

	public string DebugPrintHill => PrintHill();

	public string PrintHill()
	{
		var sb = new StringBuilder();
		var bounds = lookup.Bounds;

		for (int z = bounds.StartZ; z < bounds.EndZ; z++)
		{
			sb.AppendLine();
			for (int x = bounds.StartX; x < bounds.EndX; x++)
			{
				if (lookup.TryGetValue(new XZ(x, z), out var entry))
				{
					char c = entry.exterior ? 'x' : 'h';
					sb.Append(' ').Append(c);
				}
				else
				{
					sb.Append("  ");
				}
			}
		}

		return sb.ToString();
	}
}

public sealed class Shell
{
	private readonly Hill originalHill;
	private readonly ILookup2D<ShellEntry> lookup;
	private readonly IReadOnlyList<ShellEntry> walkaboutSteps;
	private readonly IReadOnlyList<ShellEntry> possibleCornerExtensions;

	private Shell(Hill originalHill, ILookup2D<ShellEntry> lookup,
		IReadOnlyList<ShellEntry> walkaboutSteps, IReadOnlyList<ShellEntry> possibleCornerExtensions)
	{
		this.originalHill = originalHill;
		this.lookup = lookup;
		this.walkaboutSteps = walkaboutSteps;
		this.possibleCornerExtensions = possibleCornerExtensions;
	}

	enum ShellStatus
	{
		None,

		/// <summary>
		/// Inside the original hill.
		/// </summary>
		InOriginalHill,

		/// <summary>
		/// Directly adjacent to original hill. This status wins if it is also <see cref="DiagonallyAdjacent"/>.
		/// </summary>
		DirectlyAdjacent,

		/// <summary>
		/// Diagonally adjacent to original hill, but not <see cref="DirectlyAdjacent"/>.
		/// These locs will be eligible for "corner extension"...
		/// </summary>
		DiagonallyAdjacent
	};

	enum Terior
	{
		/// <summary>
		/// Directly adjacent to an XZ that is definitely outside the shell.
		/// </summary>
		ExteriorStrong,

		/// <summary>
		/// Directly adjacent to an XZ that *might* be outside the shell.
		/// This means it is directly adjacent to at least one <see cref="ShellStatus.DiagonallyAdjacent"/> neighbor
		/// which might become part of the shell via "corner extension".
		/// </summary>
		ExteriorWeak,

		/// <summary>
		/// All of its direct neighbors are either
		/// <see cref="ShellStatus.InOriginalHill"/> or
		/// <see cref="ShellStatus.DirectlyAdjacent"/>,
		/// meaning that will definitely not be exterior when the shell becomes a hill.
		/// </summary>
		Interior,
	}

	readonly record struct ShellEntry(XZ xz, ShellStatus status, Terior blah)
	{
		/// <summary>
		/// All of these entries must be included exactly once in the walkabout.
		/// </summary>
		public bool IsKeyWalkaboutStep => status == ShellStatus.DirectlyAdjacent && blah != Terior.Interior;
	}

	public static bool TryCreate(Hill hill, [MaybeNullWhen(false)] out Shell shell)
	{
		hill.ShellHelper(out var hillLookup);
		bool inOriginalHill(XZ xz) => hillLookup.TryGetValue(xz, out _);

		ShellStatus getShellStatus(XZ xz)
		{
			if (inOriginalHill(xz))
			{
				return ShellStatus.InOriginalHill;
			}
			else if (xz.Neighbors().Any(inOriginalHill))
			{
				return ShellStatus.DirectlyAdjacent;
			}
			else if (xz.DiagonalNeighbors().Any(inOriginalHill))
			{
				return ShellStatus.DiagonallyAdjacent;
			}
			return ShellStatus.None;
		};

		var shellBounds = hillLookup.Bounds.Expand(1);
		var shellLookup = MutableLookup2D<ShellEntry>.Create(shellBounds);

		int expectedKeySteps = 0;
		XZ? walkaboutStart = null;
		List<ShellEntry> possibleCornerExtensions = new();

		foreach (var xz in shellBounds.Iterate())
		{
			var status = getShellStatus(xz);
			if (status != ShellStatus.None)
			{
				var blah = Terior.Interior;
				if (status != ShellStatus.InOriginalHill)
				{
					foreach (var neighbor in xz.Neighbors())
					{
						var sts = getShellStatus(neighbor);
						if (sts == ShellStatus.None)
						{
							blah = Terior.ExteriorStrong;
							break;
						}
						else if (sts == ShellStatus.DiagonallyAdjacent)
						{
							blah = Terior.ExteriorWeak;
						}
					}
				}

				var entry = new ShellEntry(xz, status, blah);
				shellLookup.Add(xz, entry);
				if (entry.IsKeyWalkaboutStep)
				{
					expectedKeySteps++;
					if (blah == Terior.ExteriorStrong)
					{
						walkaboutStart = xz;
					}
				}

				if (entry.status == ShellStatus.DiagonallyAdjacent)
				{
					possibleCornerExtensions.Add(entry);
				}
			}
		}

		if (!walkaboutStart.HasValue)
		{
			shell = null;
			return false;
		}

		var lookup = shellLookup.Finish();
		var walkabout = CreateWalkabout(walkaboutStart.Value, lookup);

		int actualKeySteps = walkabout.Count(step => step.IsKeyWalkaboutStep);
		if (actualKeySteps != expectedKeySteps)
		{
			var asdf = 99;
			// TODO why does this not work?
			//shell = null;
			//return false;
		}

		shell = new Shell(hill, lookup, walkabout, possibleCornerExtensions);
		return true;
	}

	/// <summary>
	/// Returns an ordered list of walkabout steps.
	/// Each step will be direct, not diagonal.
	/// Each entry that <see cref="ShellEntry.IsKeyWalkaboutStep"/> should be included exactly once,
	/// but might included zero times if the hill has an unsupported shape.
	///
	/// Entries that have <see cref="ShellStatus.DiagonallyAdjacent"/> will be included as "corner extension
	/// steps" when needed to avoid diagonal steps.
	/// In certain situations, such entries might be included more than once.
	/// </summary>
	private static List<ShellEntry> CreateWalkabout(XZ start, ILookup2D<ShellEntry> lookup)
	{
		if (!lookup.TryGetValue(start, out var currentStep) || !currentStep.IsKeyWalkaboutStep)
		{
			throw new Exception("assert fail");
		}

		HashSet<XZ> keySteps = new HashSet<XZ>(); // prevent duplication and backtracking
		keySteps.Add(start);

		var allSteps = new List<ShellEntry>(); // sorted list, including corner extension steps
		allSteps.Add(currentStep);

		bool canStep(XZ xz, out ShellEntry candidate)
		{
			return lookup.TryGetValue(xz, out candidate)
				&& candidate.IsKeyWalkaboutStep
				&& !keySteps.Contains(xz);
		}

		while (true)
		{
			bool stepped = false;
			void doKeyStep(ShellEntry nextStep)
			{
				keySteps.Add(nextStep.xz);
				allSteps.Add(nextStep);
				currentStep = nextStep;
				stepped = true;
			}

			// attempt direct step
			foreach (var neighbor in currentStep.xz.Neighbors())
			{
				if (stepped) { break; }

				if (canStep(neighbor, out var nextStep))
				{
					doKeyStep(nextStep);
				}
			}

			// attempt diagonal step
			foreach (var neighbor in currentStep.xz.DiagonalNeighbors())
			{
				if (stepped) { break; }

				if (canStep(neighbor, out var nextStep))
				{
					var corner = currentStep.xz.Neighbors().Select(xz =>
					{
						if (lookup.TryGetValue(xz, out var cornerEntry)
							//&& cornerEntry.status == ShellStatus.DiagonallyAdjacent
							&& !cornerEntry.IsKeyWalkaboutStep
							// AHA, the corner doesn't have to be DiagonallyAdjacent
							// but it can't be in the original hill
							&& cornerEntry.status != ShellStatus.InOriginalHill
							&& nextStep.xz.Neighbors().Contains(xz))
						{
							return cornerEntry.AsNullable();
						}
						return null;

					}).FirstOrDefault(x => x.HasValue);

					if (corner.HasValue)
					{
						// insert nonkey step before taking key step
						allSteps.Add(corner.Value);
						doKeyStep(nextStep);
					}
				}
			}

			if (!stepped)
			{
				// direct step and diagonal step both failed, we are done
				return allSteps;
			}
		}
	}

	public sealed record HillOptions
	{
		public int CornerExtensionLimit { get; set; } = 3;

		public int MaxElevation { get; set; } = -2;
	}

	/// <summary>
	/// Something like this:
	///   Copy all points from original hill, but set elevation to 1 or something...
	///   Do the walkabout
	///     If we see N consecutive outside corners (key steps only) perform corner extension
	///   Also include all points that were in the shell but not in the walkabout (at max height for the shell, or something...)
	/// </summary>
	/// <returns></returns>
	/// <exception cref="Exception"></exception>
	public Hill CreateNewHill(HillOptions options)
	{
		int maxElevation;
		if (options.MaxElevation > 0)
		{
			maxElevation = options.MaxElevation;
		}
		else
		{
			// a negative number indicates the drop relative to the original hill
			maxElevation = originalHill.MinElevation + options.MaxElevation;
		}

		HashSet<XZ> cornerExtensions = new();
		if (options.CornerExtensionLimit > 1)
		{
			var cornerTester = new CornerTester(lookup);
			CollectMandatoryCornerExtensions(cornerExtensions, walkaboutSteps, cornerTester, options.CornerExtensionLimit);
		}

		CollectRandomCornerExtensions(cornerExtensions, this.possibleCornerExtensions,
			this.possibleCornerExtensions.Count * 2 / 5);

		int? GetElevation(XZ xz)
		{
			if (lookup.TryGetValue(xz, out var entry))
			{
				if (entry.status == ShellStatus.InOriginalHill)
				{
					return maxElevation;
				}
				else if (entry.status == ShellStatus.DirectlyAdjacent)
				{
					return maxElevation;
				}
				else if (entry.status == ShellStatus.DiagonallyAdjacent)
				{
					if (cornerExtensions.Contains(entry.xz))
					{
						// corner extension
						//return maxElevation - 1;
						return maxElevation;
					}
				}
			}
			return null;
		};

		return Hill.CreateHill(lookup.Bounds, GetElevation);
	}

	readonly struct CornerTester
	{
		private readonly ILookup2D<ShellEntry> lookup;
		public CornerTester(ILookup2D<ShellEntry> lookup)
		{
			this.lookup = lookup;
		}

		public bool IsCorner(XZ xz)
		{
			// must have at least one emptyish neighbor on both axes
			return xz.NorthSouth().Any(IsEmptyish)
				&& xz.WestEast().Any(IsEmptyish);
		}

		private bool IsEmptyish(XZ xz)
		{
			if (lookup.TryGetValue(xz, out var entry))
			{
				return entry.status == ShellStatus.DiagonallyAdjacent
					|| entry.status == ShellStatus.None;
			}
			return true;
		}
	}

	private static void CollectRandomCornerExtensions(HashSet<XZ> collector, IReadOnlyList<ShellEntry> corners, int desiredCount)
	{
		if (desiredCount > corners.Count)
		{
			throw new ArgumentException(nameof(desiredCount));
		}

		foreach (var corner in corners.OrderBy(x => Guid.NewGuid()))
		{
			if (collector.Count >= desiredCount)
			{
				return;
			}

			collector.Add(corner.xz);
		}
	}

	private static void CollectMandatoryCornerExtensions(HashSet<XZ> collector, IReadOnlyList<ShellEntry> walkabout, CornerTester cornerTester, int cornerExtensionLimit)
	{
		var currentRun = new List<ShellEntry>(cornerExtensionLimit * 2);
		bool wantCorner = true;
		void resetRun()
		{
			currentRun.Clear();
			wantCorner = true;
		}

		// need to loop around the walkabout in case a run
		foreach (var step in walkabout.Concat(walkabout.Take(cornerExtensionLimit * 2)))
		{
			bool isCorner = false;
			if (step.IsKeyWalkaboutStep)
			{
				// Only key steps can be considered corners.
				// Steps that are DiagonallyAdjacent look like corners,
				// but they do not become corners unless we promote them via corner extension.
				isCorner = cornerTester.IsCorner(step.xz);
			}

			if (!isCorner)
			{
				if (!wantCorner)
				{
					// We were expecting a non-corner here.
					// The next one must be a corner to continue the current run.
					currentRun.Add(step);
					wantCorner = true;
				}
				else
				{
					resetRun();
				}
			}
			else
			{
				if (!wantCorner)
				{
					// This means we hit a 2nd corner without a noncorner step in between.
					// In the below example, this happen when we reach the 5 step following the "1 2 . 4 5" path.
					// Steps 4 and 5 should not be considered "consecutive corners", but note that 5
					// might be (and is) the start of a new run (the 5 . 7 8 9 run).
					//      . 4 5 .
					//    1 2 h h 7
					//    h h h h 8 9
					//    h h h h h h
					resetRun();
					currentRun.Add(step);
					wantCorner = false;
				}
				else
				{
					currentRun.Add(step);
					wantCorner = false;
				}
			}

			if (currentRun.Count >= cornerExtensionLimit * 2 - 1)
			{
				int backtrackCount = currentRun.Count - 1;
				int keyStepCounter = 0;
				bool found = false;

				while (backtrackCount >= 0)
				{
					var candidate = currentRun[backtrackCount];
					if (candidate.status == ShellStatus.DiagonallyAdjacent)
					{
						found = true;
						collector.Add(candidate.xz);
						break;
					}
					else if (candidate.IsKeyWalkaboutStep)
					{
						keyStepCounter++;
					}

					backtrackCount--;
				}

				if (found)
				{
					if (keyStepCounter > 2)
					{
						// TODO yep something is definitely wrong here..
						//throw new Exception("assert fail: keyStepCounter > 2");
					}

					resetRun();
					// Hmm, I'm not sure about this logic...
					// But consider this example:
					//        . 2 1
					//        4 h h
					//    . 6 5 h h
					//    8 h h h h
					// If the corner limit is 3, then when we accumulate "2 . 4 5 6" we will have 3 corners.
					// From backtracking to the only "." available, the keyStepCounter will be 2,
					// having counted steps 6 and 4.
					// Notice that after performing the "2 . 4" corner extension, we still need to
					// consider step 6 as a corner potentially starting a new run.
					//
					// Let's increase the corner limit to 4 and see if we can get keyStepCounter > 2... Here:
					//        . 2 1
					//        4 h h
					//      6 5 h h
					//    8 7 h h h
					//    h h h h h
					// AHA this is not possible because step 6 must be written as "." instead of "6".
					// So I think it is not possible for keyStepCounter to be > 2 here.

					if (keyStepCounter == 2)
					{
						currentRun.Add(step);
						wantCorner = false;
					}
				}
				else
				{
					// TODO this can happen if you get something like this:
					//         . 2 3 4 .
					//         1 h h h 5
					//
					// When you have "2 3 4" in the list, it does go "corner, non-corner, corner"
					// but this isn't what we mean...
					//throw new Exception("assert fail: couldn't find corner extension point");
					resetRun();
					if (cornerTester.IsCorner(step.xz))
					{
						currentRun.Add(step);
						wantCorner = false;
					}
				}
			}
		}
	}
}

// hill -> shell -> shell
// hill -> shell -> corner extensions -> walkabout -> hill

public sealed class ChunkGrid
{
	private readonly int[] chunkIds;
	private readonly int width;
	public int ChunkCount { get; }

	private ChunkGrid(int[] chunkIds, int width, int chunkCount)
	{
		this.chunkIds = chunkIds;
		this.width = width;
		this.ChunkCount = chunkCount;
	}

	public bool TryGetChunkId(Offset offset, out int chunkId)
	{
		int index = offset.OZ * width + offset.OX;
		chunkId = chunkIds[index];
		return chunkId >= 0;
	}

	public static bool TryReadChunkGrid(byte[] buffer, out ChunkGrid chunkGrid)
	{
		return TryReadChunkGrid(buffer.AsSpan(0x24C7C1, 0x1000 * 2), out chunkGrid);
	}

	/// <summary>
	/// Sapphire: The chunk grid starts at 0x24C7C1. Empty chunks are 0xFFFF.
	/// Valid chunks have the value be their index in the block data area.
	/// There's space for 0x1000 chunks, or a grid of 64x64 chunks.
	/// </summary>
	public static bool TryReadChunkGrid(ReadOnlySpan<byte> chunkGrid, out ChunkGrid grid)
	{
		int xMin = int.MaxValue;
		int xMax = int.MinValue;
		int zMin = int.MaxValue;
		int zMax = int.MinValue;
		var tempDict = new Dictionary<XZ, int>();

		for (int z = 0; z < 64; z++)
		{
			for (int x = 0; x < 64; x++)
			{
				int addr = (z * 64 + x) * 2;
				int val = chunkGrid.ReadNumber(addr, 2);
				if (val < 0xFFFF)
				{
					tempDict[new XZ(x, z)] = val;
					xMin = Math.Min(xMin, x);
					xMax = Math.Max(xMax, x);
					zMin = Math.Min(zMin, z);
					zMax = Math.Max(zMax, z);
				}
			}
		}

		var width = 1 + xMax - xMin;
		var height = 1 + zMax - zMin;
		int[] chunkIds = new int[width * height];

		for (int z = zMin; z <= zMax; z++)
		{
			for (int x = xMin; x <= xMax; x++)
			{
				int offset = (z - zMin) * width + (x - xMin);

				if (tempDict.TryGetValue(new XZ(x, z), out int chunkId))
				{
					chunkIds[offset] = chunkId;
				}
				else
				{
					chunkIds[offset] = -1;
				}
			}
		}

		grid = new ChunkGrid(chunkIds, width, tempDict.Count);
		return true;
	}
}
