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

	public static bool TryDeserialize(string json, [MaybeNullWhen(false)] out Project project)
	{
		try
		{
			project = Newtonsoft.Json.JsonConvert.DeserializeObject<Project>(json);
			return project != null;
		}
		catch (Exception)
		{
			project = null;
			return false;
		}
	}

	public string Serialize()
	{
		return Newtonsoft.Json.JsonConvert.SerializeObject(this);
	}
}

sealed class ProjectParams
{
	public required string? SDPath { get; set; }
}
