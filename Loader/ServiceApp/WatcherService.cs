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
		logger.Info("OnStart called, ignoring: {0}", string.Join(" ", args));
	}

	protected override void Dispose(bool disposing)
	{
		watcher.Dispose();
	}
}
