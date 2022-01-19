package unittests.testmethods;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class TestManagement {

	public static List<String> expected(String path, int testID) {
		try {
			return Files.readAllLines(Paths.get(path + "/EXPECTED/" + testID + ".pc"));
		} catch (IOException e) {
			e.printStackTrace();
			throw new AssertionError("Not all tests could be completed.");
		}
	}

	public static List<String> input(String path, int testID) {
		try {
			return Files.readAllLines(Paths.get(path + "/INPUT/" + testID + ".pc"));
		} catch (IOException e) {
			e.printStackTrace();
			throw new AssertionError("Not all tests could be completed.");
		}
	}

}
