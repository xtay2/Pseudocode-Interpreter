package unittests.testmethods;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public abstract class Testable {

	protected final String path = getClass().getPackageName().replace(".", "\\");

	protected final long testcount = testCount();

	private long testCount() {
		try {
			long exp = Files.list(Paths.get(path + "\\EXPECTED")).count();
			long inp = Files.list(Paths.get(path + "\\INPUT")).count();
			if (exp == inp)
				return exp;
			else
				throw new AssertionError("There should be exactly the same amount of inputs as expected outputs.\nPath: " + path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		throw new AssertionError("Couldn't find directory");
	}
}
