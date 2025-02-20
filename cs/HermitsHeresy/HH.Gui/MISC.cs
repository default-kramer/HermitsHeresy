using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace HH.Gui;

sealed class RelayCommand : ICommand
{
	private readonly Predicate<object?> _canExecute;
	private readonly Action<object?> _execute;

	public RelayCommand(Predicate<object?> canExecute, Action<object?> execute)
	{
		_canExecute = canExecute;
		_execute = execute;
	}

	public event EventHandler? CanExecuteChanged
	{
		add => CommandManager.RequerySuggested += value;
		remove => CommandManager.RequerySuggested -= value;
	}

	public bool CanExecute(object? parameter)
	{
		return _canExecute(parameter);
	}

	public void Execute(object? parameter)
	{
		_execute(parameter);
	}
}

public sealed class NodeKindVM
{
	public required string ShortName { get; init; }
	public required Func<ScriptNodeVM> maker { get; init; }

	private NodeKindVM() { }

	public ScriptNodeVM MakeNode() => maker();

	// statements
	public static readonly NodeKindVM DoNothing = new()
	{
		ShortName = "do-nothing",
		maker = () => new DoNothingVM(),
	};
	public static readonly NodeKindVM Cond = new()
	{
		ShortName = "decision",
		maker = () => new CondVM(),
	};
	public static readonly NodeKindVM Begin = new()
	{
		ShortName = "begin",
		maker = () => new BeginVM(),
	};
	public static readonly NodeKindVM SetBlock = new()
	{
		ShortName = "set-block!",
		maker = () => new SetBlockVM(),
	};

	// booleans
	public static readonly NodeKindVM False = new()
	{
		ShortName = "false",
		maker = () => new FalseVM(),
	};
	public static readonly NodeKindVM True = new()
	{
		ShortName = "true",
		maker = () => new TrueVM(),
	};
	public static readonly NodeKindVM InHill = new()
	{
		ShortName = "in-hill?",
		maker = () => new InHillVM(),
	};
	public static readonly NodeKindVM InArea = new()
	{
		ShortName = "in-area?",
		maker = () => new InAreaVM(),
	};
	public static readonly NodeKindVM BlockMatches = new()
	{
		ShortName = "block-matches?",
		maker = () => new BlockMatchesVM(),
	};
	public static readonly NodeKindVM ChiselMatches = new()
	{
		ShortName = "chisel-matches?",
		maker = () => new ChiselMatchesVM(),
	};

	// other
	public static readonly NodeKindVM CondItem = new()
	{
		ShortName = "decision-item",
		maker = () => new CondItemVM(),
	};
}

public class INPC : INotifyPropertyChanged
{
	public event PropertyChangedEventHandler? PropertyChanged;

	const string invalidPropertyName = "invalid property name";
	protected void RaisePropertyChanged([CallerMemberName] string propertyName = invalidPropertyName)
	{
		if (propertyName == invalidPropertyName)
		{
			throw new ArgumentException($"Did CallerMemberName fail? Got: {propertyName}");
		}
		PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
	}
}
