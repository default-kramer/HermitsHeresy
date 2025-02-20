using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace HH.Gui;

public partial class ScriptEditorTreeNode : UserControl, ScriptNodeWrapper.IParentFinder
{
	private static readonly IReadOnlyList<Brush> brushPool = [
		new SolidColorBrush(Color.FromRgb(255, 230, 230)),
		new SolidColorBrush(Color.FromRgb(230, 255, 230)),
		new SolidColorBrush(Color.FromRgb(233, 233, 255)),
	];
	private static readonly IReadOnlyList<Brush> darkerBrushPool = [
		new SolidColorBrush(Color.FromRgb(255, 180, 180)),
		new SolidColorBrush(Color.FromRgb(180, 255, 180)),
		new SolidColorBrush(Color.FromRgb(180, 180, 255)),
	];
	private int brushIndex = 0;

	public ScriptEditorTreeNode()
	{
		InitializeComponent();

		CheckHover();
		this.border.MouseEnter += (s, e) => OnMouseEnter();
		this.border.MouseLeave += (s, e) => OnMouseLeave();
		this.border.MouseDown += (s, e) => OnMouseDown();
		this.DataContextChanged += (s, e) => OnDataContextChanged();
	}

	private void OnDataContextChanged()
	{
		if (DataContext is ScriptNodeWrapper vm)
		{
			vm.SetParentFinder(this);
		}
	}

	ScriptNodeWrapper? ScriptNodeWrapper.IParentFinder.FindParent()
	{
		var parentElement = this.Ancestors().OfType<ScriptEditorTreeNode>().FirstOrDefault();
		return parentElement?.DataContext as ScriptNodeWrapper;
	}

	protected override void OnVisualParentChanged(DependencyObject oldParent)
	{
		base.OnVisualParentChanged(oldParent);

		var parent = this.Ancestors().OfType<ScriptEditorTreeNode>().FirstOrDefault();
		if (parent != null)
		{
			this.brushIndex = (parent.brushIndex + 1) % brushPool.Count;
			CheckHover();
		}
	}

	/// <summary>
	/// Holds the set of descendent nodes for which <see cref="hasMouse"/> is true.
	/// If this set becomes empty and this node still <see cref="hasMouse"/> then
	/// it becomes the hovered node.
	/// </summary>
	private readonly HashSet<ScriptEditorTreeNode> childrenWithMouse = new();

	/// <summary>
	/// When this is true, it will also be true for all ancestors.
	/// </summary>
	private bool hasMouse = false;

	/// <summary>
	/// This should only be true for a single node (and not its ancestors).
	/// </summary>
	private bool isHover = false;

	/// <summary>
	/// When a node is selected (clicked), *all* its ancestors will hold a reference to
	/// that selected node via this field.
	/// This means that if a different node becomes selected anywhere in the tree, that information
	/// can bubble up to some common ancestor (worst case: the root node) which can then tell
	/// the previously selected node to become unselected.
	/// </summary>
	private ScriptEditorTreeNode? selectedNode = null;

	private void AddChildWithMouse(ScriptEditorTreeNode child)
	{
		childrenWithMouse.Add(child);
		CheckHover();
	}

	private void RemoveChildWithMouse(ScriptEditorTreeNode child)
	{
		childrenWithMouse.Remove(child);
		CheckHover();
	}

	private void CheckHover()
	{
		if (hasMouse && childrenWithMouse.Count == 0)
		{
			isHover = true;
			border.Background = darkerBrushPool[brushIndex];
		}
		else
		{
			isHover = false;
			border.Background = brushPool[brushIndex];
		}
	}

	private void OnMouseEnter()
	{
		hasMouse = true;
		CheckHover();
		foreach (var parent in this.Ancestors().OfType<ScriptEditorTreeNode>())
		{
			parent.AddChildWithMouse(this);
		}
	}

	private void OnMouseLeave()
	{
		hasMouse = false;
		CheckHover();
		foreach (var parent in this.Ancestors().OfType<ScriptEditorTreeNode>())
		{
			parent.RemoveChildWithMouse(this);
		}
	}

	private void OnMouseDown()
	{
		if (!isHover) { return; }

		if (this.DataContext is ScriptNodeWrapper node)
		{
			node.IsSelected = true;
		}

		this.UpdateSelectedNode(this);

		foreach (var parent in this.Ancestors().OfType<ScriptEditorTreeNode>())
		{
			parent.UpdateSelectedNode(this);
		}
	}

	private void Unselect()
	{
		if (this.DataContext is ScriptNodeWrapper node)
		{
			node.IsSelected = false;
		}
	}

	private void UpdateSelectedNode(ScriptEditorTreeNode selectedNode)
	{
		if (selectedNode != this.selectedNode)
		{
			this.selectedNode?.Unselect();
			this.selectedNode = selectedNode;
			SelectedNodeChanged?.Invoke(this, new SelectedNodeChangedEventArgs(selectedNode));
		}
	}

	public sealed class SelectedNodeChangedEventArgs : EventArgs
	{
		public ScriptEditorTreeNode SelectedNode { get; }
		public object? SelectedItem => SelectedNode.DataContext;

		public SelectedNodeChangedEventArgs(ScriptEditorTreeNode selectedNode)
		{
			this.SelectedNode = selectedNode;
		}
	}

	public event EventHandler<SelectedNodeChangedEventArgs>? SelectedNodeChanged;
}
