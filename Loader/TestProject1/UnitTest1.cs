using HH.Core;
using System.Text;

namespace TestProject1
{
	[TestClass]
	public class UnitTest1
	{
		private DirectoryInfo FindTestProjectRoot()
		{
			var dir = new DirectoryInfo(Directory.GetCurrentDirectory());
			while (dir != null)
			{
				if (dir.Name == "TestProject1")
				{
					return dir;
				}
				dir = dir.Parent;
			}
			throw new Exception("Couldn't find test root dir");
		}

		private Stgdat LoadStgdat(string path)
		{
			var dir = FindTestProjectRoot();
			var file = Path.Combine(dir.FullName, path);
			var rawContent = System.IO.File.ReadAllBytes(file);
			var helper = new StgdatHelper.IoA(rawContent);
			return new Stgdat(helper);
		}

		[TestMethod]
		public void test001()
		{
			// just a random STB file I had available...
			var stgdat = LoadStgdat("testfiles/001-STGDAT01.BIN");
			Assert.IsTrue(stgdat.IsFragmented);
		}

		[TestMethod]
		public void test002()
		{
			// ... the same file as 001, but I loaded and immediately saved it in DQB2.
			// I'm still not sure whether DQB2 *always* performs this defragmentation,
			// but let this serve as proof that it happens at least *some* of the time.
			var stgdat = LoadStgdat("testfiles/002-STGDAT01.BIN");

			Assert.IsFalse(stgdat.IsFragmented);
			Assert.IsTrue(stgdat.ParseItems(out var items));
			Assert.AreEqual(15829, items.Count);
		}

		[TestMethod]
		public void test003()
		{
			// Just another defragmented file but now I've learned to always save the CMNDAT too.
			var stgdat = LoadStgdat("testfiles/003/STGDAT01.BIN");

			Assert.IsFalse(stgdat.IsFragmented);
			Assert.IsTrue(stgdat.ParseItems(out var items));
			Assert.AreEqual(15856, items.Count);
		}

		[TestMethod]
		public void DeleteSomeItems()
		{
			Assert.Inconclusive("WARNING: This test saves to B00, uncomment this line if you want to run it");

			var stgdat = LoadStgdat("testfiles/003/STGDAT01.BIN");

			Assert.IsFalse(stgdat.IsFragmented);
			Assert.IsTrue(stgdat.ParseItems(out var items));
			Assert.AreEqual(15856, items.Count);

			var tuples = stgdat.ComputeItemDimensions().ToList();
			Assert.AreEqual(5146, tuples.Count);

			var hammer = tuples.Single(x => x.Item.BlockVal == 2047 && x.Item.ItemVal == 1410);
			var greenTablet = tuples.Single(x => x.Item.BlockVal == 2047 && x.Item.ItemVal == 2565);
			var blueTablet = tuples.Single(x => x.Item.BlockVal == 2047 && x.Item.ItemVal == 2569);

			stgdat.RemoveAllItemsExcept(hammer, greenTablet, blueTablet);

			var output = stgdat.Export();
			File.WriteAllBytes(@"C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD\B00\STGDAT01.BIN", output);
		}
	}
}
