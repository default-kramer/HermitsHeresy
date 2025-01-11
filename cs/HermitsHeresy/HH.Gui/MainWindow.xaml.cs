using System.IO;
using System.Windows;

namespace HH.Gui
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		private readonly ProjectManagementViewmodel projectManagementVM = new();

		public MainWindow()
		{
			InitializeComponent();
			this.DataContext = projectManagementVM;
			this.rootse.SelectedNodeChanged += Rootse_SelectedNodeChanged;
		}

		private void Rootse_SelectedNodeChanged(object? sender, ScriptEditorTreeNode.SelectedNodeChangedEventArgs e)
		{
			this.nodeDetailEditor.DataContext = e.SelectedItem;
		}
	}
}