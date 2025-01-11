using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace HH.Gui;

sealed class ProjectViewmodel
{
	public ProjectParamsViewmodel ProjectParamsVM { get; init; } = new();

	public ScriptNodeWrapper ScriptRootNode { get; init; }

	public ProjectViewmodel()
	{
		var root = new BeginVM();
		var cond = root.AddCond();
		var item1 = cond.AddCondItem();
		var item2 = cond.AddCondItem();
		item1.TestExpr = new ScriptNodeWrapper(new TrueVM());
		this.ScriptRootNode = new ScriptNodeWrapper(root);
	}

	public ProjectViewmodel(SerializationModel.Project model) : this()
	{
		if (model.ProjectParams != null)
		{
			this.ProjectParamsVM = new(model.ProjectParams);
		}
	}

	public SerializationModel.Project ToSerializationModel()
	{
		return new SerializationModel.Project()
		{
			ProjectParams = ProjectParamsVM.ToSerializationModel(),
		};
	}
}

sealed class ProjectParamsViewmodel : INPC
{
	private string sdPath = "";
	public string SDPath
	{
		get { return sdPath; }
		private set { sdPath = value; RaisePropertyChanged(); }
	}

	public ICommand ChangeSDCommand { get; }

	public ProjectParamsViewmodel()
	{
		SDPath = TryFindSD()?.FullName ?? "";
		ChangeSDCommand = new RelayCommand(_ => true, ChangeSD);
	}

	public ProjectParamsViewmodel(SerializationModel.ProjectParams model) : this()
	{
		SDPath = model.SDPath ?? SDPath;
	}

	private void ChangeSD(object? arg)
	{
		var ofd = new Microsoft.Win32.OpenFolderDialog();
		ofd.Multiselect = false;

		string startDir = SDPath;
		if (string.IsNullOrWhiteSpace(startDir))
		{
			startDir = TryFindSD()?.FullName ?? "";
		}
		if (startDir.Length > 0)
		{
			ofd.InitialDirectory = startDir;
		}

		var result = ofd.ShowDialog();
		if (result.GetValueOrDefault())
		{
			SDPath = ofd.FolderName;
		}
	}

	internal static DirectoryInfo? TryFindSD()
	{
		var homepath = Environment.GetEnvironmentVariable("HOMEPATH");
		var username = Environment.GetEnvironmentVariable("USERNAME");

		string path;
		if (homepath != null)
		{
			path = homepath;
		}
		else if (username != null)
		{
			path = Path.Combine("Users", username);
		}
		else
		{
			return null;
		}

		path = Path.Combine(path, "Documents\\My Games\\DRAGON QUEST BUILDERS II\\Steam\\76561198073553084\\SD");

		var homedrive = Environment.GetEnvironmentVariable("HOMEDRIVE") ?? "C:";
		var dir = new DirectoryInfo(homedrive + path);
		if (dir.Exists)
		{
			return dir;
		}
		return null;
	}

	public SerializationModel.ProjectParams ToSerializationModel()
	{
		return new SerializationModel.ProjectParams()
		{
			SDPath = this.SDPath,
		};
	}
}
