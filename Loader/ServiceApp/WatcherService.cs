using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceProcess;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

class WatcherService : ServiceBase
{
	private Dispatcher? dispatcher = null;

	protected override void OnStart(string[] args)
	{
		dispatcher = new Dispatcher();
		dispatcher.StartWatchers();
	}

	protected override void Dispose(bool disposing)
	{
		dispatcher?.Dispose();
	}
}
