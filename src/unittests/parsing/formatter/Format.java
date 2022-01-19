package unittests.parsing.formatter;

import static unittests.testmethods.TestManagement.expected;
import static unittests.testmethods.TestManagement.input;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

import parsing.formatter.Formatter;
import unittests.testmethods.Testable;

/**
 * Testet mehrere unformatierte programme gegen die erwarteten Formatierungen.
 */
public class Format extends Testable {

	/** Prints the differences between two programs with line indices. */
	void printDifferences(List<String> formatted, List<String> expected) {
		List<Integer> wrongLines = new ArrayList<>();
		System.out.println("\nFormatted:");
		System.out.flush();
		for (int i = 0; i < Math.max(formatted.size(), expected.size()); i++) {
			if (i >= formatted.size()) {
				System.err.println(i + ": MISSING");
				wrongLines.add(i);
			} else if (!formatted.get(i).equals(expected.get(i))) {
				System.err.println(i + ": " + formatted.get(i));
				wrongLines.add(i);
			}
		}
		System.err.flush();
		System.out.println("\nExpected:");
		wrongLines.stream().forEach(i -> {
			System.out.println(i + ": " + expected.get(i));
		});
	}

	/**
	 * Executes a specific test and prints possible differences in the results.
	 */
	void test(int testNr, List<String> formatted, List<String> expected) {
		System.out.println("Test " + testNr);
		// Should be equal to the expected.
		List<String> reformatted = Formatter.format(expected);
		if (!reformatted.equals(expected)) {
			printDifferences(reformatted, expected);
			throw new AssertionError("Program gets formatted twice in Test: " + testNr);
		}
		if (!formatted.equals(expected)) {
			printDifferences(formatted, expected);
			throw new AssertionError("Formatting-Difference in Test " + testNr);
		}
	}

	@Test
	void testAll() {
		for (int i = 1; i <= testcount; i++)
			test(i, Formatter.format(input(path, i)), expected(path, i));
	}
}
