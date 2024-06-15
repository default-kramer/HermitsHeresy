using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp.Work;

abstract class ReadFileWork : WorkItem
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

	private readonly string fullPath;
	private int retryCount;

	public ReadFileWork(string fullPath, int retryCount = 3)
	{
		this.fullPath = fullPath;
		this.retryCount = retryCount;
	}

	public sealed override void Execute(WorkQueue queue)
	{
		if (FileVersion.TryReadBytes(fullPath, out var fileVersion, out var bytes))
		{
			logger.Debug("Read all bytes of {0}", fileVersion);
			OnReadFileCompleted(queue, fileVersion, bytes);
		}
		else if (retryCount > 0)
		{
			// File is probably still locked, try again in one second
			retryCount--;
			queue.AddWork(this, delay: TimeSpan.FromSeconds(1));
		}
	}

	protected abstract void OnReadFileCompleted(WorkQueue queue, FileVersion fileVersion, byte[] bytes);
}
