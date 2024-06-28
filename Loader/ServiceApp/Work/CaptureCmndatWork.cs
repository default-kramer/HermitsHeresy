using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp.Work;

class CaptureCmndatWork : ReadFileWork
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

	private readonly CmndatManager cmndatManager;

	public CaptureCmndatWork(string fullPath, CmndatManager cmndatManager) : base(fullPath)
	{
		this.cmndatManager = cmndatManager;
	}

	protected override void OnReadFileCompleted(WorkQueue queue, FileVersion cmndatVersion, byte[] cmndatBytes)
	{
		cmndatManager.CaptureCmndat(cmndatVersion, cmndatBytes);
		logger.Info("Captured CMNDAT file {0}", cmndatVersion);
		cmndatManager.ProcessArchiveRequests();
	}
}
