using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HH.Gui.SerializationModel;

sealed class Project
{
	public required ProjectParams? ProjectParams { get; set; }
	public required ISerializedScriptNode? ScriptRoot { get; set; }

	private static JsonSerializerSettings MakeSettings() => new JsonSerializerSettings() { TypeNameHandling = TypeNameHandling.All };

	public static bool TryDeserialize(string json, [MaybeNullWhen(false)] out Project project)
	{
		try
		{
			project = Newtonsoft.Json.JsonConvert.DeserializeObject<Project>(json, MakeSettings());
			return project != null;
		}
		catch (Exception ex)
		{
			project = null;
			return false;
		}
	}

	public string Serialize()
	{
		return Newtonsoft.Json.JsonConvert.SerializeObject(this, MakeSettings());
	}
}

sealed class ProjectParams
{
	public required string? SDPath { get; set; }
}
