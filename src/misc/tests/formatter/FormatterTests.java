package misc.tests.formatter;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import formatter.basic.Formatter;

public class FormatterTests {

	@Test
	void formatter() throws IOException {
		int testAmount = new File("Testcases/Formatter/Original/").list().length;
		for (int i = 0; i < testAmount; i++) {
			String testName = "Test" + i + ".pc";
			List<String> input = Files.readAllLines(Paths.get("Testcases/Formatter/Original/" + testName));
			List<String> expected = Files.readAllLines(Paths.get("Testcases/Formatter/Expected/" + testName));
			List<String> result = Formatter.format(input, true);
			compare(result, expected, i);
			// Format again
			result = Formatter.format(result, true);
			compare(result, expected, i);
		}
	}

	/**
	 * Compares two programs and throws a detailed error if they are different.
	 */
	private void compare(List<String> result, List<String> expected, int testNr) {
		if (result.size() != expected.size()) {
			print(result, expected);
			throw new AssertionError("The formatted program for test " + testNr + " had a different length, than expected.");
		}
		for (int i = 0; i < result.size(); i++) {
			assertNotEquals(null, result.get(i));
			if (!expected.get(i).equals(result.get(i))) {
				print(result, expected);
				throw new AssertionError("Discrepancy in line " + i + ".");
			}
		}
	}

	private void print(List<String> result, List<String> expected) {
		System.out.println("RESULT" + "-".repeat(50));
		print(result);
		System.out.println("EXPECTED" + "-".repeat(50));
		print(expected);
		System.out.println("-".repeat(50));
	}

	private void print(List<String> prog) {
		prog.stream().forEach(System.out::println);
	}
}
