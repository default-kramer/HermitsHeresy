using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Data;

namespace HH.Gui;

public interface IBlockSelectionListener
{
	void OnSelectionChanged(BlockChoiceVM choice);
}

public class BlockChoiceVM : INPC
{
	public required IReadOnlyList<int> BlockIds { get; init; }
	public required string Name { get; init; }
	public required IBlockSelectionListener Listener { get; init; }

	private bool isSelected;
	public bool IsSelected
	{
		get { return isSelected; }
		set
		{
			if (value != isSelected)
			{
				isSelected = value;
				RaisePropertyChanged();
				Listener.OnSelectionChanged(this);
			}
		}
	}

	public string DisplayName => $"{Name} ({string.Join(',', BlockIds)})";
}

public abstract class BlockSelectViewmodel : INPC, IBlockSelectionListener
{
	public CollectionViewSource FilterBlocks { get; }
	public IReadOnlyList<BlockChoiceVM> blocks { get; }
	private readonly HashSet<BlockChoiceVM> selectedBlocks = new();

	/// <summary>
	/// Makes a new list so you can deselect some blocks without breaking the iterator state.
	/// </summary>
	protected IReadOnlyList<BlockChoiceVM> CurrentlySelectedBlocks() => selectedBlocks.ToList();

	private string searchFilter = "";
	public string SearchFilter
	{
		get { return searchFilter; }
		set
		{
			searchFilter = value;
			RaisePropertyChanged();
			FilterBlocks.View.Refresh();
		}
	}

	public BlockSelectViewmodel()
	{
		blocks = BuildChoices();
		FilterBlocks = new CollectionViewSource();
		FilterBlocks.Source = blocks;
		FilterBlocks.Filter += FilterBlocks_Filter;
		FilterBlocks.View.Refresh();
	}

	protected abstract IReadOnlyList<BlockChoiceVM> BuildChoices();

	private void FilterBlocks_Filter(object sender, FilterEventArgs e)
	{
		var vm = (BlockChoiceVM)e.Item;
		e.Accepted = MatchesFilter(vm, searchFilter);
	}

	private static bool MatchesFilter(BlockChoiceVM vm, string filter)
	{
		var words = filter.Split(' ', StringSplitOptions.RemoveEmptyEntries);
		if (words.Length == 0)
		{
			return true;
		}

		foreach (var word in words)
		{
			if (int.TryParse(word, out var blockId))
			{
				if (!vm.BlockIds.Contains(blockId))
				{
					return false;
				}
			}
			else
			{
				if (!vm.Name.ToLowerInvariant().Contains(word.ToLowerInvariant()))
				{
					return false;
				}
			}
		}

		return true;
	}

	public void OnSelectionChanged(BlockChoiceVM choice)
	{
		bool changed;
		if (choice.IsSelected)
		{
			changed = selectedBlocks.Add(choice);
		}
		else
		{
			changed = selectedBlocks.Remove(choice);
		}

		if (changed)
		{
			OnBlockSelectionChanged(choice);
		}
	}

	protected virtual void OnBlockSelectionChanged(BlockChoiceVM block) { }

	public List<List<int>> BuildSelectionLists()
	{
		return selectedBlocks.Select(block => block.BlockIds.ToList()).ToList();
	}

	public void RestoreFromSelectionLists(IReadOnlyList<IReadOnlyList<int>> selections)
	{
		foreach (var sequence in selections)
		{
			var found = blocks.FirstOrDefault(b => b.BlockIds.SequenceEqual(sequence));
			if (found != null)
			{
				found.IsSelected = true;
			}
			else
			{
				// TODO could do some fuzzy matching here, to preserve intent if the
				// blockdata is tweaked to reclassify stuff... but maybe it's better not to?
			}
		}
	}
}

public class BlockMultiselectViewmodel : BlockSelectViewmodel
{
	protected override IReadOnlyList<BlockChoiceVM> BuildChoices()
	{
		var dyed = Blockdata.AllBlockdatas.SelectMany(block => block.Dyes.Select(dye => (block, dye))).Select(item => new BlockChoiceVM()
		{
			Name = $"{item.block.Name} [{item.dye.Color}]",
			BlockIds = [item.dye.BlockId],
			Listener = this,
		});
		var undyed = Blockdata.AllBlockdatas.GroupBy(x => x.PrimaryBlockId).Select(group => new BlockChoiceVM()
		{
			Name = group.First().Name,
			BlockIds = group.Select(x => x.BlockId).ToList(),
			Listener = this,
		});
		return dyed.Concat(undyed).OrderBy(x => x.Name).ToList();
	}

	protected override void OnBlockSelectionChanged(BlockChoiceVM block)
	{
		RaisePropertyChanged(nameof(SelectedBlocksString));
	}

	public string SelectedBlocksString => string.Join(',', CurrentlySelectedBlocks().Select(x => x.Name).OrderBy(x => x));
}

public class BlockSingleSelectViewmodel : BlockSelectViewmodel
{
	protected override IReadOnlyList<BlockChoiceVM> BuildChoices()
	{
		List<BlockChoiceVM> choices = new();

		foreach (var block in Blockdata.AllBlockdatas.Where(b => b.BlockId == b.PrimaryBlockId))
		{
			choices.Add(new BlockChoiceVM()
			{
				Name = block.Name,
				BlockIds = [block.PrimaryBlockId],
				Listener = this,
			});

			foreach (var dye in block.Dyes)
			{
				choices.Add(new BlockChoiceVM()
				{
					Name = $"{block.Name} [{dye.Color}]",
					BlockIds = [dye.BlockId],
					Listener = this,
				});
			}
		}

		choices.Sort((a, b) => a.Name.CompareTo(b.Name));
		return choices;
	}

	protected override void OnBlockSelectionChanged(BlockChoiceVM block)
	{
		if (block.IsSelected)
		{
			SelectedBlock = block;

			foreach (var other in CurrentlySelectedBlocks().Where(b => b != block))
			{
				other.IsSelected = false;
			}

			RaisePropertyChanged(nameof(SelectedBlockString));
		}
	}

	private BlockChoiceVM? SelectedBlock = null;

	private int? directEntryBlockId = null;
	public string DirectEntryBlockId
	{
		get { return directEntryBlockId?.ToString() ?? ""; }
		set
		{
			if (int.TryParse(value, out int blockId))
			{
				directEntryBlockId = blockId;
				if (SelectedBlock != null)
				{
					SelectedBlock.IsSelected = false;
					SelectedBlock = null;
				}
			}
			else
			{
				directEntryBlockId = null;
			}

			RaisePropertyChanged();
			RaisePropertyChanged(nameof(EnableListSelection));
			RaisePropertyChanged(nameof(SelectedBlockString));
		}
	}

	public bool EnableListSelection => !directEntryBlockId.HasValue;
	public string SelectedBlockString => directEntryBlockId?.ToString() ?? SelectedBlock?.DisplayName ?? "";

	public int? SelectedBlockId => directEntryBlockId ?? SelectedBlock?.BlockIds?.Single();

	public void SetSelectedBlock(int blockId)
	{
		var found = blocks.FirstOrDefault(b => b.BlockIds.Contains(blockId));
		if (found != null)
		{
			directEntryBlockId = null;
			found.IsSelected = true;
		}
		else
		{
			DirectEntryBlockId = blockId.ToString();
		}
	}
}
