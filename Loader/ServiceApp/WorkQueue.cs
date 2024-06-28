using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

/// <summary>
/// See <see cref="WorkQueue"/>.
/// </summary>
abstract class WorkItem
{
	public abstract void Execute(WorkQueue queue);

	public virtual void OnException(Exception ex, WorkQueue queue, out bool rethrow)
	{
		rethrow = false;
	}
}

/// <summary>
/// Ensures that work items are executed serially.
/// The goal is to reduce the possibility of race conditions in <see cref="WorkItem"/> code.
/// </summary>
sealed class WorkQueue : IDisposable
{
	private static readonly NLog.Logger logger = NLog.LogManager.GetCurrentClassLogger();

	sealed class Entry
	{
		private readonly WorkItem workItem;

		public Entry(WorkItem workItem)
		{
			this.workItem = workItem;
		}

		public DateTime NotBefore { get; set; } = DateTime.MinValue;

		public void Execute(WorkQueue workQueue)
		{
			workItem.Execute(workQueue);
		}

		public void OnException(Exception ex, WorkQueue workQueue, out bool rethrow)
		{
			workItem.OnException(ex, workQueue, out rethrow);
		}
	}

	private readonly ConcurrentQueue<Entry> queue = new();
	private bool stop = false;

	private WorkQueue() { }

	public static WorkQueue StartNewWorker()
	{
		var queue = new WorkQueue();
		var thread = new Thread(queue.DoWork) { IsBackground = true }; // does IsBackground even do anything?
		thread.Start();
		return queue;
	}

	public void AddWork(WorkItem work, TimeSpan? delay = null)
	{
		logger.Debug("Adding to work queue: {0}", work);

		var entry = new Entry(work);
		if (delay != null)
		{
			entry.NotBefore = DateTime.UtcNow.Add(delay.Value);
		}
		queue.Enqueue(entry);
	}

	public void Dispose() => Shutdown();

	public void Shutdown()
	{
		stop = true;
		logger.Info("Stopping work queue");
	}

	private void DoWork()
	{
		while (!stop)
		{
			List<Entry> delayEntries = new();

			while (queue.TryDequeue(out var entry))
			{
				if (entry.NotBefore > DateTime.UtcNow)
				{
					delayEntries.Add(entry);
				}
				else
				{
					try
					{
						entry.Execute(this);
					}
					catch (Exception ex)
					{
						logger.Error(ex, "A work item failed {0}", entry);
						entry.OnException(ex, this, out bool rethrow);
						if (rethrow)
						{
							throw;
						}
					}
				}
			}

			foreach (var entry in delayEntries)
			{
				queue.Enqueue(entry);
			}

			Thread.Sleep(1000);
		}
	}
}
