using NLog;
using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceProcess;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

class WatcherService : ServiceBase
{
	private static readonly Logger logger = LogManager.GetCurrentClassLogger();
	private readonly SDWatcher watcher;

	public WatcherService(DirectoryInfo sd)
	{
		watcher = new SDWatcher(sd);
	}

	protected override void OnStart(string[] args)
	{
		logger.Info("OnStart called, ignoring args: {0}", string.Join(" ", args));
	}

	protected override void OnStop()
	{
		logger.Info("OnStop called, nothing to do");
	}

	protected override void OnShutdown()
	{
		logger.Info("OnShutdown called, nothing to do");
	}

	protected override void Dispose(bool disposing)
	{
		logger.Info("Disposing...");
		watcher.Dispose();
	}
}
