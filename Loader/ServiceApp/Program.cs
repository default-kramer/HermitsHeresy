using HH.Core;
using NLog;
using ServiceApp;
using System.ServiceProcess;

class Program
{
	public static void Main(string[] args)
	{
		string sd = $@"C:\Users\{Environment.UserName}\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD";
		if (args.Length > 0)
		{
			sd = args[0];
		}

		try
		{
			Start(new DirectoryInfo(sd));
		}
		catch (Exception ex)
		{
			// NLog probably isn't available
			try
			{
				File.WriteAllText("./_HH-CRASH.txt", ex.ToString());
			}
			catch { }
			throw;
		}
	}

	private static void Start(DirectoryInfo sd)
	{
		if (!sd.Exists)
		{
			throw new Exception($"The given SD does not exist: {sd.FullName}");
		}
		var hh = sd.CreateSubdirectory(".hermits-heresy");
		if (!hh.Exists)
		{
			throw new Exception($"Failed to create: {hh.FullName}");
		}

		const string gdcKey = "nlog-gdc-logdir"; // greppable
		GlobalDiagnosticsContext.Set(gdcKey, Path.Combine(hh.FullName, "nlogs"));
		LogManager.Configuration = LogManager.Configuration.Reload();
		LogManager.ReconfigExistingLoggers();
		var logger = LogManager.GetCurrentClassLogger();
		logger.Info("Started NLog, SD is {0}", sd.FullName);

		try
		{
			Start2(sd);
		}
		catch (Exception ex)
		{
			logger.Fatal(ex);
			throw;
		}
	}

	private static void Start2(DirectoryInfo sd)
	{
		if (!Environment.UserInteractive)
		{
			using var service = new WatcherService(sd);
			ServiceBase.Run(service);
		}
		else
		{
			//var bytes = System.IO.File.ReadAllBytes(@"C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD\B00\STGDAT01.BIN");
			//var stgdat = new Stgdat(new StgdatHelper.IoA(bytes));
			//using var conn = ConnectionFactory.OpenDatabase(@"C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD\.hermits-heresy\test.db");
			//var loader = new Loader(stgdat, conn);
			//loader.LoadBlocksOnly();

			Console.WriteLine("User interactive mode");
			using var watcher = new SDWatcher(sd);
			Console.WriteLine("Press ESC to stop...");
			while (Console.ReadKey(true).Key != ConsoleKey.Escape) { }
		}
	}
}
