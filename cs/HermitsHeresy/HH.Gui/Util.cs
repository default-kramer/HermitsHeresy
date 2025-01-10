using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using System.Windows;

namespace HH.Gui;

static class Util
{
	public static DependencyObject? GetParent(this DependencyObject obj)
	{
		if (obj == null)
		{
			return null;
		}
		if (obj is ContentElement ce)
		{
			var parent = ContentOperations.GetParent(ce);
			if (parent != null)
			{
				return parent;
			}
			if (obj is FrameworkContentElement fce)
			{
				return fce.Parent;
			}
			return null;
		}
		return VisualTreeHelper.GetParent(obj);
	}

	public static IEnumerable<DependencyObject> Ancestors(this DependencyObject obj)
	{
		var parent = obj.GetParent();
		while (parent != null)
		{
			yield return parent;
			parent = GetParent(parent);
		}
	}
}
