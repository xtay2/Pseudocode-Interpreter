package misc.helper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import interpreting.modules.parser.Parser.IdxLine;

/**
 * Abstract Helperclass that contains all functions that get used all over the project.
 */
public final class StringHelper {

	private StringHelper() {
		// Dead constructor
	}

	/**
	 * Points to a char in a string.
	 * 
	 * <pre>
	 * pointUnderline("Hello", 2) produces:
	 * Hello
	 *   ^
	 * </pre>
	 * 
	 * @param line    is the full line
	 * @param pointer is the index of the char that gets pointed at.
	 */
	public static String pointUnderline(String line, int pointer) {
		return pointUnderline(line, pointer, 1);
	}

	/**
	 * Points to a section in a string.
	 * 
	 * <pre>
	 * pointUnderline("Hello", 2, 2) produces:
	 * Hello
	 *   ^^
	 * </pre>
	 * 
	 * @param line          is the full line
	 * @param pointer       is the index of the char that gets pointed at.
	 * @param pointerLength is the length of the pointed section.
	 */
	public static String pointUnderline(String line, int pointer, int pointerLength) {
		return line + "\n" + " ".repeat(pointer) + "^".repeat(pointerLength);
	}

	/**
	 * Removes the char at the specified index and returns the line with lenght -1. If the idx is out of
	 * bounds, the unchanged line gets returned.
	 */
	public static String removeCharAt(int idx, String line) {
		return new StringBuilder(line).deleteCharAt(idx).toString();
	}

	/**
	 * An alternative toString() method for every {@link Collection}.
	 * 
	 * <pre>
	 * For example [1, 2, 3] becomes:
	 * -1
	 * -2
	 * -3
	 * </pre>
	 */
	public static String enumerate(Collection<?> collection) {
		return collection.stream().map(e -> e.toString()).reduce("", (acc, e) -> acc + "\n-" + e);
	}

	/**
	 * Takes a program and adds an incrementing index to each line.
	 * 
	 * @see IdxLine
	 */
	public static List<IdxLine> indexLines(List<String> lines) {
		List<IdxLine> indexedLines = new ArrayList<>();
		for (int i = 0; i < lines.size(); i++)
			indexedLines.add(new IdxLine(lines.get(i), i + 1));
		return indexedLines;
	}

	/**
	 * Takes an indexed program and simplifies it back to just the strings.
	 * 
	 * The returned {@link List} is immutable, because information is lost on the way, and it should
	 * only get used for analyzing purposes.
	 */
	public static List<String> unIndexLines(List<IdxLine> lines) {
		return lines.stream().map(e -> e.line()).toList();
	}
}
