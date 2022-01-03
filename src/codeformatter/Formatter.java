package codeformatter;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * Methods that are tagged with this annotation can potentially interfere with
 * strings.
 */
@Retention(RetentionPolicy.SOURCE)
@interface InterferesWithStrings {
}

public class Formatter {

	private static ArrayList<String> rawProgram;

	public static ArrayList<String> format(ArrayList<String> p) {
		rawProgram = p;
		stripTrailing();
		lineBreakBetweenBlocks(0);
		indent();
//		moveImportsUp();
//		correctSpaces();
//		addMissingMain();
		return rawProgram;
	}

	/** Remove trailing whitespaces for all lines */
	private static void stripTrailing() {
		for (int i = 0; i < rawProgram.size(); i++)
			rawProgram.set(i, rawProgram.get(i).stripTrailing());
	}

	/** Distribute blockbrackets to multiple lines. */
	@InterferesWithStrings
	private static void lineBreakBetweenBlocks(int start) {
		for (int i = start; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			// Mehr als eine geschlossene Klammer
			if ((line.chars().filter(ch -> ch == '}').count() > 1
					// oder es gibt nur eine geschlossene klammer, die aber nicht alleine steht
					|| (line.contains("}") && line.stripIndent().length() > 1))
					// und diese klammer(n) nicht in einem String ist
					&& isNotInString(line.indexOf('}'), line)) {
				rawProgram.set(i, rawProgram.get(i).replaceFirst("}", ""));
				rawProgram.add(i + 1, "}");
				lineBreakBetweenBlocks(i > 2 ? i - 2 : 0);
				return;
			}
			int firstOBr = line.indexOf("{");
			if (firstOBr != -1 && isNotInString(firstOBr, line)) {
				if (firstOBr != line.length() - 1) {
					rawProgram.add(i + 1, rawProgram.get(i).substring(firstOBr + 1, line.length()));
					rawProgram.set(i, rawProgram.get(i).substring(0, firstOBr + 1));
				}
				else if (line.stripIndent().charAt(0) == '{') {
					rawProgram.remove(i);
					rawProgram.set(i - 1, rawProgram.get(i - 1) + " {");
				}
			}
		}
	}

	/** Add correct tabwise indentation. */
	private static void indent() {
		int brack = 0;
		for (int i = 0; i < rawProgram.size(); i++) {
			String s = rawProgram.get(i);
			if (s.indexOf('}') != -1)
				brack--;
			rawProgram.set(i, "\t".repeat(brack) + s.stripIndent());
			if (s.indexOf('{') != -1)
				brack++;
		}
	}

	/** Move all import statements to the top of the file */
	private static void moveImportsUp() {
		ArrayList<String> imports = new ArrayList<>();
		for (int i = 0; i < rawProgram.size(); i++) {
			if (rawProgram.get(i).startsWith("import")) {
				imports.add(rawProgram.get(i));
				rawProgram.remove(i);
			}
		}
		Collections.sort(imports, Comparator.reverseOrder());
		for (String imp : imports)
			rawProgram.add(0, imp);
	}

	/**
	 * Corrects the spaces around commatas and single-char arithmetic Operators.
	 * 
	 * @see {@link Formatter#isArithmeticOperator}
	 */
	@InterferesWithStrings
	private static void correctSpaces() {
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);

			// Entferne alle mehrfachen spaces.
			while (line.contains("  "))
				line = line.replaceAll("  ", " ");

			// Entferne alle spaces vor kommatas.
			for (int j = 1; j < line.length(); j++) {
				if (line.charAt(j) == ',' && line.charAt(j - 1) == ' ')
					line = removeCharAt(line, j - 1);
			}

			// Füge ein space hinter jedem kommatas ein
			for (int j = 0; j < line.length() - 1; j++) {
				if (line.charAt(j) == ',' && line.charAt(j + 1) != ' ')
					line = insertCharAt(' ', line, j + 1);
			}

			// Padding für single-char Operators
			for (int j = 1; j < line.length() - 1; j++) {
				// Arithmetische Operatoren
				if (isArithmeticOperator(line.charAt(j), line.charAt(j - 1), line.charAt(j + 1))) {
					if (line.charAt(j - 1) != ' ') {
						line = insertCharAt(' ', line, j);
						j++;
					}
					if (line.charAt(j + 1) != ' ') {
						line = insertCharAt(' ', line, j + 1);
						j++;
					}
				}
				// Klammern (), [] und ^
				if ((line.charAt(j) == ')' || line.charAt(j) == ']' || line.charAt(j) == '^') && line.charAt(j - 1) == ' ') {
					line = removeCharAt(line, j - 1);
					j--;
				}
				if ((line.charAt(j) == '(' || line.charAt(j) == '[' || line.charAt(j) == '^') && line.charAt(j + 1) == ' ') {
					line = removeCharAt(line, j + 1);
					j--;
				}
			}
			rawProgram.set(i, line);
		}

	}

	/**
	 * Returns true for +, -, *, /, %, and ÷. Returns false for ->.
	 */
	private static boolean isArithmeticOperator(char op, char lastChar, char nextChar) {
		if (op == '-' && nextChar != ' ')
			return false;
		return op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '÷';
	}

	/**
	 * Adds a main-function if there isn't one already present.
	 */
	private static void addMissingMain() {
		for (String line : rawProgram) {
			if (line.stripIndent().startsWith("main"))
				return;
		}
		ArrayList<String> newProg = new ArrayList<String>();
		rawProgram = newProg;
		rawProgram.add(0, "main:");
		rawProgram.add(1, "\texit()");
		rawProgram.add(2, "");
	}

	/**
	 * Insert a char into a string at a given index.
	 * 
	 * @return the new string with the inserted char.
	 */
	private static String insertCharAt(char c, String s, int i) {
		return s.substring(0, i) + c + s.substring(i);
	}

	/**
	 * Remove a char from a string at a given index.
	 * 
	 * @return the new string without the deleted char.
	 */
	private static String removeCharAt(String s, int i) {
		return s.substring(0, i) + s.substring(i + 1);
	}

	/**
	 * Tells, if a char at a specified index is not in the string boundaries. ("")
	 * 
	 * @return true if the index is not in a string.
	 */
	public static boolean isNotInString(int index, String line) {
		boolean inString = false;
		for (int i = 0; i < index; i++) {
			if (inString && line.charAt(i) == '\\')
				i++;
			else if (line.charAt(i) == '"')
				inString = !inString;
		}
		return !inString && index != -1;
	}

}
