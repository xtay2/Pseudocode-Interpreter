package misc.helper;

import java.util.Collection;
import java.util.List;

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
	 * @param line is the full line
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
	 * @param line is the full line
	 * @param pointer is the index of the char that gets pointed at.
	 * @param pointerLength is the length of the pointed section.
	 */
	public static String pointUnderline(String line, int pointer, int pointerLength) {
		int leadingTabs = 0;
		for (int i = 0; i < line.length(); i++) {
			if (line.charAt(i) == '\t')
				leadingTabs++;
			else
				break;
		}
		return line.stripLeading() + "\n" + " ".repeat(pointer - leadingTabs) + "^".repeat(pointerLength);
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
		if (collection.isEmpty())
			return "<none>";
		return collection.stream().map(e -> e.toString()).reduce("", (acc, e) -> acc + "\n-" + e);
	}

	/**
	 * An alternative toString() method for every {@link List}.
	 *
	 * <pre>
	 * For example [1, 2, 3] becomes:
	 * 0: 1
	 * 1: 2
	 * 2: 3
	 * </pre>
	 */
	public static String enumerateIndexed(List<?> list) {
		String res = "";
		for (int i = 0; i < list.size(); i++)
			res += i + ": " + list.get(i) + (i == list.size() - 1 ? "" : "\n");
		return res;
	}

}
