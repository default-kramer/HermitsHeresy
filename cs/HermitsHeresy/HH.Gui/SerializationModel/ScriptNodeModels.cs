using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HH.Gui.SerializationModel;

public interface ISerializedScriptNode
{
	ScriptNodeWrapper Reconstruct();
}

public sealed class FalseNode : ISerializedScriptNode
{
	public ScriptNodeWrapper Reconstruct() => new ScriptNodeWrapper(new FalseVM());
}


public sealed class TrueNode : ISerializedScriptNode
{
	public ScriptNodeWrapper Reconstruct() => new ScriptNodeWrapper(new TrueVM());
}

public sealed class SetBlockNode : ISerializedScriptNode
{
	public required int? SelectedBlockId { get; init; }

	public ScriptNodeWrapper Reconstruct() => new ScriptNodeWrapper(new SetBlockVM(this));
}

public sealed class DoNothingNode : ISerializedScriptNode
{
	public ScriptNodeWrapper Reconstruct() => new ScriptNodeWrapper(new DoNothingVM());
}

public abstract class NodeWithChildren : ISerializedScriptNode
{
	public required List<ISerializedScriptNode> Children { get; init; }

	public abstract ScriptNodeWrapper Reconstruct();
}

public sealed class BeginNode : NodeWithChildren
{
	public override ScriptNodeWrapper Reconstruct() => new ScriptNodeWrapper(new BeginVM(this));
}

public sealed class BlockMatchesNode : ISerializedScriptNode
{
	public required List<List<int>> SelectedBlocks { get; init; }

	public ScriptNodeWrapper Reconstruct()
	{
		return new ScriptNodeWrapper(new BlockMatchesVM(this));
	}
}

public sealed class CondNode : NodeWithChildren
{
	public override ScriptNodeWrapper Reconstruct() => new ScriptNodeWrapper(new CondVM(this));
}

public sealed class CondItemNode : ISerializedScriptNode
{
	public required ISerializedScriptNode TestExpr { get; init; }
	public required ISerializedScriptNode BodyExpr { get; init; }

	public ScriptNodeWrapper Reconstruct()
	{
		return new ScriptNodeWrapper(new CondItemVM(this));
	}
}