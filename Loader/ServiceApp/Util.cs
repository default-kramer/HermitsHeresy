using System;
using System.Collections.Generic;
using System.Data.SQLite;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ServiceApp;

static class Util
{
	public static SQLiteParameter AddParam(this SQLiteCommand command)
	{
		var param = command.CreateParameter();
		command.Parameters.Add(param);
		return param;
	}
}
