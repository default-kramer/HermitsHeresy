using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HH.Gui;

public sealed class Blockdata
{
	public sealed class Dye
	{
		public required int BlockId { get; init; }
		public required string Color { get; init; }
	}

	public required string Name { get; init; }
	public required int BlockId { get; init; }
	public required int PrimaryBlockId { get; init; }
	public required bool IsLiquid { get; init; }
	public required IReadOnlyList<Dye> Dyes { get; init; }


	public static readonly IReadOnlyList<Blockdata> AllBlockdatas;

	static Blockdata()
	{
		string json = System.IO.File.ReadAllText("Blockdata.json");
		AllBlockdatas = Newtonsoft.Json.JsonConvert.DeserializeObject<List<Blockdata>>(json)
			?? throw new Exception("Failed to read blockdata");
	}
}
