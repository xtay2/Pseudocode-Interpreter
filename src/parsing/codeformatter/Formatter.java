package parsing.codeformatter;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import exceptions.parsing.IllegalCodeFormatException;
import helper.Helper;
import parsing.program.KeywordType;

/**
 * Methods that are tagged with this annotation can potentially interfere with
 * strings. They should use the function Formatter#isNotInString
 */
@Retention(RetentionPolicy.SOURCE)
@interface InterferesWithStrings {
}

public class Formatter {

	private static List<String> rawProgram;

	public static List<String> format(List<String> p) {
		rawProgram = p;
		stripTrailing();
		lineBreakBetweenBlocks(0);
		indent();
		addMissingSemicolon();
		correctSpaces();
		addMissingMain();
		moveImportsUp();
		checkForLonelyBrackets();
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
					&& Helper.isNotInString(line.indexOf('}'), line)) {
				rawProgram.set(i, rawProgram.get(i).replaceFirst("}", ""));
				rawProgram.add(i + 1, "}");
				lineBreakBetweenBlocks(i > 2 ? i - 2 : 0);
				return;
			}
			int firstOBr = line.indexOf("{");
			if (firstOBr != -1 && Helper.isNotInString(firstOBr, line)) {
				if (firstOBr != line.length() - 1) {
					rawProgram.add(i + 1, rawProgram.get(i).substring(firstOBr + 1, line.length()));
					rawProgram.set(i, rawProgram.get(i).substring(0, firstOBr + 1));
				} else if (line.stripIndent().charAt(0) == '{') {
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
			if (brack < 0)
				throw new IllegalCodeFormatException(i, "There are more closed than open brackets.");
			rawProgram.set(i, "\t".repeat(brack) + s.stripIndent());
			if (s.indexOf('{') != -1)
				brack++;
		}
	}

	/**
	 * Adds a semicolon behind each one line statement thats missing one.
	 */
	private static void addMissingSemicolon() {
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			for (int j = 0; j < line.length(); j++) {
				if (line.charAt(j) == ':' && Helper.isNotInString(j, line) && line.charAt(line.length() - 1) != ';') {
					rawProgram.set(i, line + ";");
					return;
				}
			}
		}
	}

	/** Move all import statements to the top of the file */
	private static void moveImportsUp() {
		ArrayList<String> imports = new ArrayList<>();
		for (int i = 0; i < rawProgram.size(); i++) {
			if (rawProgram.get(i).startsWith(KeywordType.IMPORT.keyword)) {
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
			while (Helper.isNotInString(line.indexOf("  "), line))
				line = line.replaceAll("  ", " ");

			// Entferne alle spaces vor kommatas.
			for (int j = 1; j < line.length(); j++) {
				if (Helper.isNotInString(j, line) && line.charAt(j) == ',' && line.charAt(j - 1) == ' ')
					line = removeCharAt(line, j - 1);
			}

			// Füge ein space hinter jedem komma/doppelpunkt ein
			for (int j = 0; j < line.length() - 1; j++) {
				if (Helper.isNotInString(j, line) && (line.charAt(j) == ',' || line.charAt(j) == ':') && line.charAt(j + 1) != ' ')
					line = insertCharAt(' ', line, j + 1);
			}

			// Padding für single-char Operators
			for (int j = 1; j < line.length() - 1; j++) {
				if (Helper.isNotInString(j, line)) {
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
					// Klammern innerhalb space entfernen (), [] und ^
					if ((line.charAt(j) == ')' || line.charAt(j) == ']' || line.charAt(j) == '^') && line.charAt(j - 1) == ' ') {
						line = removeCharAt(line, j - 1);
						j--;
					}
					if ((line.charAt(j) == '(' || line.charAt(j) == '[' || line.charAt(j) == '^') && line.charAt(j + 1) == ' ') {
						line = removeCharAt(line, j + 1);
						j--;
					}
					if ((line.charAt(j + 1) == '{') && line.charAt(j) != ' ') {
						line = insertCharAt(' ', line, j + 1);
						j++;
					}
				}
			}
			rawProgram.set(i, line);
		}

	}

	/**
	 * Returns true for +, -, *, /, %. Returns false for ->.
	 */
	private static boolean isArithmeticOperator(char op, char lastChar, char nextChar) {
		if (op == '-' && nextChar != ' ')
			return false;
		return op == '+' || op == '-' || op == '*' || op == '/' || op == '%';
	}

	/**
	 * Adds a main-function if there isn't one already present.
	 */
	private static void addMissingMain() {
		for (String line : rawProgram) {
			if (line.stripIndent().startsWith("main"))
				return;
		}
		rawProgram.add(0, "main: exit();");
		rawProgram.add(1, "");
	}

	/** Throw an exeption if a bracket exists that doesn't get closed. */
	private static void checkForLonelyBrackets() {
		int simple = 0, curly = 0, square = 0;
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			for (int j = 0; j < line.length(); j++) {
				char c = line.charAt(j);
				if (Helper.isNotInString(j, line)) {
					if (c == '(')
						simple++;
					if (c == '{')
						curly++;
					if (c == '[')
						square++;
					if (c == ')')
						simple--;
					if (c == '}')
						curly--;
					if (c == ']')
						square--;
					if (simple < 0 || curly < 0 || square < 0)
						throw new IllegalCodeFormatException(i, "There exists atleast one unopened bracket.");
				}
			}
		}
		if (simple != 0 || curly != 0 || square != 0) {

			throw new IllegalCodeFormatException("There exists atleast one unclosed bracket." + "\nUnclosed simple brackets: " + simple
					+ "\nUnclosed curly brackets: " + curly + "\nUnclosed square brackets: " + square);
		}
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
}