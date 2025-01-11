using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;

namespace HH.Gui;

sealed class ProjectManagementViewmodel : INPC
{
	private string lastSavedJson = "";

	private ProjectViewmodel projectVM = new();
	public ProjectViewmodel ProjectVM
	{
		get { return projectVM; }
		set { projectVM = value; RaisePropertyChanged(); }
	}

	const string projectFilter = "Hermit's Heresy project files|*.hhproj";
	const string projectExtension = ".hhproj";

	private string projectFilename = "";
	public string ProjectFilename
	{
		get { return projectFilename; }
		set { projectFilename = value; RaisePropertyChanged(""); }
	}

	public bool HasProject => ProjectFilename.Length > 0;

	public Visibility HasProject_Visibility => HasProject ? Visibility.Visible : Visibility.Collapsed;

	public ICommand OpenProjectCommand { get; }
	public ICommand SaveProjectCommand { get; }
	public ICommand CreateNewProjectCommand { get; }
	public ICommand CloseProjectCommand { get; }

	public ProjectManagementViewmodel()
	{
		OpenProjectCommand = new RelayCommand(_ => true, OpenProject);
		SaveProjectCommand = new RelayCommand(_ => HasProject, SaveProject);
		CreateNewProjectCommand = new RelayCommand(_ => true, CreateNewProject);
		CloseProjectCommand = new RelayCommand(_ => HasProject, CloseProject);
	}

	private bool HasUnsavedChanges()
	{
		return HasProject
			&& lastSavedJson != ProjectVM.ToSerializationModel().Serialize();
	}

	private void CloseProject(object? arg)
	{
		// This may not be great UX, but it's the best I can think of.
		// The Open and Create New actions should not allow the user to discard unsaved
		// changes without any warning/confirmation. But if a simple OK/Cancel confirmation would
		// then allow them to proceed directly to the Open or Save File Dialog it
		// seems easy to be confused as to what you are doing: are you saving your unsaved
		// changes or are you discarding your changes and choosing the new file for the Open/Create action?
		// So instead, if unsaved changes exist, we will tell the user "You must save or close this project first."
		// Then it is clear that the confirmation means "confirm you want to close this project without saving".
		// Once the project is closed or saved, Open and Create New are no longer destructive and
		// do not require any confirmation to proceed.

		bool shouldClose = true;
		if (HasUnsavedChanges())
		{
			var result = MessageBox.Show("Close project without saving?", "Discard Unsaved Changes", MessageBoxButton.OKCancel,
				MessageBoxImage.Warning, defaultResult: MessageBoxResult.Cancel);

			shouldClose = result == MessageBoxResult.OK;
		}

		if (shouldClose)
		{
			ProjectVM = new();
			ProjectFilename = "";
		}
	}

	private bool WarnAboutUnsavedChanges()
	{
		if (HasUnsavedChanges())
		{
			MessageBox.Show("Current project has unsaved changes. Save or close this project first.");
			return true;
		}
		return false;
	}

	private void OpenProject(object? arg)
	{
		if (WarnAboutUnsavedChanges())
		{
			return;
		}

		var ofd = new Microsoft.Win32.OpenFileDialog();
		ofd.Filter = projectFilter;
		var result = ofd.ShowDialog();
		if (result.GetValueOrDefault())
		{
			var json = System.IO.File.ReadAllText(ofd.FileName);
			if (SerializationModel.Project.TryDeserialize(json, out var project))
			{
				ProjectVM = new ProjectViewmodel(project);
				lastSavedJson = json;
				ProjectFilename = ofd.FileName;
			}
			else
			{
				MessageBox.Show("Cannot load project file.", "Invalid Project File", MessageBoxButton.OK, MessageBoxImage.Error);
			}
		}
	}

	private void CreateNewProject(object? arg)
	{
		if (WarnAboutUnsavedChanges())
		{
			return;
		}

		DoSaveProject(_ => { });
	}

	private void SaveProject(object? arg)
	{
		DoSaveProject(sfd =>
		{
			sfd.FileName = ProjectFilename;
			sfd.InitialDirectory = Path.GetDirectoryName(sfd.FileName);
		});
	}

	private void DoSaveProject(Action<Microsoft.Win32.SaveFileDialog> customizeSFD)
	{
		var sfd = new Microsoft.Win32.SaveFileDialog();
		sfd.DefaultExt = projectExtension;
		sfd.Filter = projectFilter;
		customizeSFD(sfd);
		var result = sfd.ShowDialog();
		if (result.GetValueOrDefault())
		{
			var json = ProjectVM.ToSerializationModel().Serialize();
			System.IO.File.WriteAllText(sfd.FileName, json);
			ProjectFilename = sfd.FileName;
			lastSavedJson = json;
		}
	}
}
