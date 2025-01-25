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

public class BlockMultiselectViewmodel : INPC, IBlockSelectionListener
{
	public CollectionViewSource FilterBlocks { get; }
	public IReadOnlyList<BlockChoiceVM> blocks { get; }
	private readonly HashSet<BlockChoiceVM> selectedBlocks = new();

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

	public string SelectedBlocksString => string.Join(',', selectedBlocks.Select(x => x.Name).OrderBy(x => x));

	public BlockMultiselectViewmodel()
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
		blocks = dyed.Concat(undyed).OrderBy(x => x.Name).ToList();

		FilterBlocks = new CollectionViewSource();
		FilterBlocks.Source = blocks;
		FilterBlocks.Filter += FilterBlocks_Filter;
		FilterBlocks.View.Refresh();
	}

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
			RaisePropertyChanged(nameof(SelectedBlocksString));
		}
	}

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
