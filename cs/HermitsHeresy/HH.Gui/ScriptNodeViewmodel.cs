using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace HH.Gui;

/// <summary>
/// This class handles shared functionality including relocating a node within its parent's
/// list of children and replacing a node with a different (but compatible) node.
/// </summary>
public class ScriptNodeWrapper : INPC
{
	private readonly Dictionary<NodeKindVM, ScriptNodeVM> memory = new();

	public ScriptNodeWrapper(ScriptNodeVM content)
	{
		SelectedContent = content;
		selectedContent = SelectedContent; // to avoid the warning
		CommandMoveDown = new RelayCommand(_ => Parent?.CanMoveChildDown(this) ?? false, _ => Parent?.MoveChildDown(this));
		CommandMoveUp = new RelayCommand(_ => Parent?.CanMoveChildUp(this) ?? false, _ => Parent?.MoveChildUp(this));
	}

	private ScriptNodeVM selectedContent;
	public ScriptNodeVM SelectedContent
	{
		get { return selectedContent; }
		set
		{
			selectedContent = value;
			memory[selectedContent.Kind] = selectedContent;
			RaisePropertyChanged();
		}
	}

	private bool isSelected;
	public bool IsSelected
	{
		get { return isSelected; }
		set { isSelected = value; RaisePropertyChanged(); }
	}

	public IEnumerable<NodeKindVM> KindChoices => selectedContent.KindChoices;

	public NodeKindVM SelectedKind
	{
		get { return selectedContent.Kind; }
		set
		{
			if (SelectedKind != value)
			{
				if (memory.TryGetValue(value, out var vm))
				{
					SelectedContent = vm;
				}
				else
				{
					SelectedContent = value.MakeNode();
				}
				RaisePropertyChanged();
			}
		}
	}

	public interface IParentFinder
	{
		ScriptNodeWrapper? FindParent();
	}

	private IParentFinder? parentFinder;

	/// <summary>
	/// This should be set by the UI when its DataContext is set to this viewmodel.
	/// </summary>
	/// <remarks>
	/// My first instinct was to maintain two-way parent-child references within these
	/// viewmodels, but that approach actually creates more opportunities for bugs/anomalies.
	/// Instead, the viewmodels will hold one-way references (parent holds children)
	/// and this <see cref="IParentFinder"/> mechanism will be used when children need
	/// to find their parents.
	/// </remarks>
	public void SetParentFinder(IParentFinder parentFinder)
	{
		this.parentFinder = parentFinder;
		RaisePropertyChanged(""); // notify all properties
	}

	public ScriptNodeWrapper? Parent
	{
		get
		{
			var finder = parentFinder ?? throw new InvalidOperationException("ParentFinder is not set");
			return finder.FindParent();
		}
	}

	public ICommand CommandMoveDown { get; }
	public ICommand CommandMoveUp { get; }

	protected virtual IList<ScriptNodeWrapper> ChildList => SelectedContent.Children;

	protected virtual bool CanMoveChildDown(ScriptNodeWrapper child)
	{
		return CanMoveHelper(ChildList, child, 1, out _, out _);
	}

	protected virtual bool CanMoveChildUp(ScriptNodeWrapper child)
	{
		return CanMoveHelper(ChildList, child, -1, out _, out _);
	}

	protected virtual void MoveChildDown(ScriptNodeWrapper child)
	{
		MoveHelper(ChildList, child, 1);
	}

	protected virtual void MoveChildUp(ScriptNodeWrapper child)
	{
		MoveHelper(ChildList, child, -1);
	}

	private static bool CanMoveHelper(IList<ScriptNodeWrapper> children, ScriptNodeWrapper child, int delta, out int srcIndex, out int dstIndex)
	{
		srcIndex = -1;
		dstIndex = -1;
		srcIndex = children.IndexOf(child);
		if (srcIndex < 0)
		{
			return false;
		}
		dstIndex = srcIndex + delta;
		return dstIndex >= 0 && dstIndex < children.Count;
	}

	private static void MoveHelper(IList<ScriptNodeWrapper> children, ScriptNodeWrapper child, int delta)
	{
		if (CanMoveHelper(children, child, delta, out int src, out int dst))
		{
			children.RemoveAt(src);
			children.Insert(dst, child);
		}
	}
}

/// <summary>
/// Base class for all Script Node viewmodels.
/// </summary>
public abstract class ScriptNodeVM : INPC
{
	protected ObservableCollection<ScriptNodeWrapper> children = new();

	public IList<ScriptNodeWrapper> Children => children;

	public abstract NodeKindVM Kind { get; }

	public string ShortName => Kind.ShortName;

	/// <summary>
	/// The complete list of kinds that could replace this node.
	/// </summary>
	public IEnumerable<NodeKindVM> KindChoices => PossibleKinds();

	/// <summary>
	/// See <see cref="KindChoices"/>
	/// </summary>
	protected abstract IEnumerable<NodeKindVM> PossibleKinds();
}

public abstract class StatementVM : ScriptNodeVM
{
	protected override IEnumerable<NodeKindVM> PossibleKinds()
	{
		yield return NodeKindVM.SetBlock;
		yield return NodeKindVM.Cond;
		yield return NodeKindVM.Begin;
		yield return NodeKindVM.DoNothing;
	}
}

public abstract class BooleanVM : ScriptNodeVM
{
	protected override IEnumerable<NodeKindVM> PossibleKinds()
	{
		yield return NodeKindVM.BlockMatches;
		yield return NodeKindVM.ChiselMatches;
		yield return NodeKindVM.InHill;
		yield return NodeKindVM.InArea;
		yield return NodeKindVM.False;
		yield return NodeKindVM.True;
	}
}

public class FalseVM : BooleanVM
{
	public override NodeKindVM Kind => NodeKindVM.False;
}

public class TrueVM : BooleanVM
{
	public override NodeKindVM Kind => NodeKindVM.True;
}

public class BeginVM : StatementVM
{
	public override NodeKindVM Kind => NodeKindVM.Begin;

	public CondVM AddCond()
	{
		var node = new CondVM();
		var wrapper = new ScriptNodeWrapper(node);
		children.Add(wrapper);
		return node;
	}

	public DoNothingVM AddDoNothing()
	{
		var node = new DoNothingVM();
		var wrapper = new ScriptNodeWrapper(node);
		children.Add(wrapper);
		return node;
	}
}

public class CondVM : StatementVM
{
	public CondItemVM AddCondItem()
	{
		var node = new CondItemVM();
		var wrapper = new ScriptNodeWrapper(node);
		children.Add(wrapper);
		return node;
	}

	public override NodeKindVM Kind => NodeKindVM.Cond;
}

public class DoNothingVM : StatementVM
{
	public override NodeKindVM Kind => NodeKindVM.DoNothing;
}

public class SetBlockVM : StatementVM
{
	private int block = 11;
	public int Block
	{
		get { return block; }
		set { block = value; RaisePropertyChanged(); }
	}

	public override NodeKindVM Kind => NodeKindVM.SetBlock;
}

public class BlockMatchesVM : BooleanVM
{
	private int block = 11;
	public int Block
	{
		get { return block; }
		set { block = value; RaisePropertyChanged(); }
	}

	public override NodeKindVM Kind => NodeKindVM.BlockMatches;
}

public class ChiselMatchesVM : BooleanVM
{
	public override NodeKindVM Kind => NodeKindVM.ChiselMatches;
}

public class CondItemVM : ScriptNodeVM
{
	private ScriptNodeWrapper _testExpr = new ScriptNodeWrapper(new FalseVM());
	public ScriptNodeWrapper TestExpr
	{
		get { return _testExpr; }
		set { _testExpr = value; RaisePropertyChanged(); }
	}

	public ScriptNodeWrapper BodyExpr { get; }

	public CondItemVM()
	{
		var begin = new BeginVM();
		begin.AddDoNothing();
		BodyExpr = new ScriptNodeWrapper(begin);
	}

	public override NodeKindVM Kind => NodeKindVM.CondItem;

	protected override IEnumerable<NodeKindVM> PossibleKinds()
	{
		yield return NodeKindVM.CondItem;
	}
}

public class InHillVM : BooleanVM
{
	private string filepath = "";
	public string Filepath
	{
		get { return filepath; }
		set { filepath = value; RaisePropertyChanged(); }
	}

	public override NodeKindVM Kind => NodeKindVM.InHill;
}

public class InAreaVM : BooleanVM
{
	private string filepath = "";
	public string Filepath
	{
		get { return filepath; }
		set { filepath = value; RaisePropertyChanged(); }
	}

	public override NodeKindVM Kind => NodeKindVM.InArea;
}
