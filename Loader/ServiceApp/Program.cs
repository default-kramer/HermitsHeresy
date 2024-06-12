using HH.Core;
using ServiceApp;
using System.ServiceProcess;

class Program
{
	public static void Main(string[] args)
	{
		if (!Environment.UserInteractive)
		{
			using var service = new WatcherService();
			ServiceBase.Run(service);
		}
		else
		{
			var bytes = System.IO.File.ReadAllBytes(@"C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD\B00\STGDAT01.BIN");
			var stgdat = new Stgdat(new StgdatHelper.IoA(bytes));
			using var conn = ConnectionFactory.OpenDatabase(@"C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD\.hermits-heresy\test.db");
			var loader = new Loader(stgdat, conn);
			loader.LoadBlocksOnly();

			/*
			Console.WriteLine("User interactive mode");
			using var dispatcher = new Dispatcher();
			dispatcher.StartWatchers();
			Console.WriteLine("Press ESC to stop...");
			while (Console.ReadKey(true).Key != ConsoleKey.Escape) { }
			*/
		}
	}
}
