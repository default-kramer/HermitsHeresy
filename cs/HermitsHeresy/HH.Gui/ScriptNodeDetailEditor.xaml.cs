﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace HH.Gui
{
	/// <summary>
	/// Interaction logic for ScriptNodeDetailEditor.xaml
	/// </summary>
	public partial class ScriptNodeDetailEditor : UserControl
	{
		public ScriptNodeDetailEditor()
		{
			InitializeComponent();

			// Will be bound, but this is needed to prevent Binding Failures
			this.DataContext = null;
		}
	}
}
