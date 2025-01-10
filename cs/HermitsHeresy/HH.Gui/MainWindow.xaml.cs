using System.IO;
using System.Windows;

namespace HH.Gui
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		private void setup()
		{
			var rootNode = new BeginVM();

			var cond = rootNode.AddCond();
			var item1 = cond.AddCondItem();
			var item2 = cond.AddCondItem();
			item1.TestExpr = new ScriptNodeWrapper(new TrueVM());

			this.rootse.DataContext = new ScriptNodeWrapper(rootNode);
			this.rootse.SelectedNodeChanged += Rootse_SelectedNodeChanged;
		}

		private void Rootse_SelectedNodeChanged(object? sender, ScriptEditorTreeNode.SelectedNodeChangedEventArgs e)
		{
			this.nodeDetailEditor.DataContext = e.SelectedItem;
		}

		public MainWindow()
		{
			InitializeComponent();
			setup();
		}

		// Actually... it doesn't make sense to open a single STGDAT file! Right?
		// It's more like you want a "project parameters" tab where you can specify more than
		// one STGDAT file (e.g. when you want to copy stuff from one stage to another).
		// Really, what we need is to Open or Save a *.hhproj (Hermit's Heresy project file).
		private void OnOpenClicked(object sender, RoutedEventArgs e)
		{
			var ofd = new Microsoft.Win32.OpenFileDialog();
			ofd.InitialDirectory = TryFindSD()?.FullName;
			ofd.Filter = "DQB2 STGDAT files|STGDAT*.BIN|All Files|*.*";
			var result = ofd.ShowDialog();
			if (result.GetValueOrDefault())
			{

			}
		}

		private static DirectoryInfo? TryFindSD()
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
	}
}