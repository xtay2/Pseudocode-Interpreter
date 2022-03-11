package interpreting.modules.formatter;

import static building.types.specific.FlagType.isFlag;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import building.expressions.main.CloseBlock;
import building.expressions.main.statements.FlagSpace;
import building.expressions.normal.brackets.OpenBlock;
import building.types.specific.BuilderType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.modules.parser.Parser;
import misc.helper.Helper;

public class Formatter {

	private static List<String> rawProgram;

	//@formatter:off
	public static final String

			/** The symbol of a OpenBlock. { */
			OB = BuilderType.OPEN_BLOCK.toString(),
	
			/** The symbol of a CloseBlock. } */
			CB = BuilderType.CLOSE_BLOCK.toString(),
			
			/** The symbol of a multi-close-scope ; */
			MCS = String.valueOf(Parser.MULTI_CLOSE_SCOPE),
			
			/** The symbol of a single-line-comment # */
			SLC = String.valueOf(Parser.SINGLE_LINE_COMMENT);
			
	public static final char
	
			/** The symbol of a one-line-start : */
			OLS = ':';
	
	//@formatter:on

	public static List<String> format(List<String> p) {
		// Strip all lines
		rawProgram = new ArrayList<>(p.stream().map(e -> e.strip()).toList());
		formatOpenScope();
		formatClosedScope();
		orderFlags();
		doSemicolons();
		correctSpaces();
		addMissingMain();
		moveImportsUp();
		checkForLonelyBrackets();
		indent();
		return rawProgram;
	}

	/**
	 * Adds a main-function if there isn't one already present.
	 */
	private static void addMissingMain() {
		final String MAIN = KeywordType.MAIN.toString();
		for (String line : rawProgram) {
			if (line.stripIndent().startsWith(MAIN))
				return;
		}
		rawProgram.add(0, MAIN + " " + OB);
		rawProgram.add(1, "\t" + SLC + "Implement me!");
		rawProgram.add(2, CB);
		rawProgram.add(3, "");
	}

	/**
	 * Adds a semicolon behind each one-line-statement or native declaration, thats missing one and
	 * remove the unnecessary ones.
	 */
	private static void doSemicolons() {
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			if (!line.isEmpty()) {
				final int idxOfOls = line.indexOf(OLS);
				if (idxOfOls != -1 && Helper.isRunnableCode(idxOfOls, line)) {
					if (!line.endsWith(MCS))
						rawProgram.set(i, line + MCS);
				} else {
					while (rawProgram.get(i).endsWith(MCS))
						rawProgram.set(i, rawProgram.get(i).substring(0, rawProgram.get(i).length() - 1));
				}
			}
		}
	}

	/** Throw an exeption if a bracket exists that doesn't get closed. */
	private static void checkForLonelyBrackets() {
		int simple = 0, curly = 0, square = 0;
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			for (int j = 0; j < line.length(); j++) {
				char c = line.charAt(j);
				if (Helper.isRunnableCode(j, line)) {
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
	 * Returns true for +, -, *, /, %. Returns false for ->, +=, -=, *=, etc...
	 */
	private static boolean checkOperator(char op, char lastChar, char nextChar) {
		if ((op == '-' && nextChar != ' ') || isSingleCharOp(op, nextChar) || isSingleCharOp(op, lastChar))
			return false;
		return isSingleCharOp(lastChar, op);

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
			while (Helper.isRunnableCode(line.indexOf("  "), line))
				line = line.replaceAll("  ", " ");

			// Entferne alle spaces vor kommatas und doppelpunkten.
			for (int j = 1; j < line.length(); j++) {
				if (Helper.isRunnableCode(j, line) && (line.charAt(j) == ',' || line.charAt(j) == OLS) && line.charAt(j - 1) == ' ')
					line = removeCharAt(line, j - 1);
			}

			// Füge ein space hinter jedem komma/doppelpunkt ein
			for (int j = 0; j < line.length() - 1; j++) {
				if (Helper.isRunnableCode(j, line) && (line.charAt(j) == ',' || line.charAt(j) == OLS) && line.charAt(j + 1) != ' ')
					line = insertCharAt(' ', line, j + 1);
			}

			// Padding für single-char Operators
			for (int j = 1; j < line.length() - 1; j++) {
				if (Helper.isRunnableCode(j, line)) {
					// Arithmetische Operatoren
					if (checkOperator(line.charAt(j), line.charAt(j - 1), line.charAt(j + 1))) {
						if (line.charAt(j - 1) != ' ') {
							line = insertCharAt(' ', line, j);
							j++;
						}
						if (line.charAt(j + 1) != ' ') {
							line = insertCharAt(' ', line, j + 1);
							j++;
						}
					}
					// Klammern innerhalb space entfernen (), []
					if ((line.charAt(j) == ')' || line.charAt(j) == ']') && line.charAt(j - 1) == ' ') {
						line = removeCharAt(line, j - 1);
						j--;
					}
					if ((line.charAt(j) == '(' || line.charAt(j) == '[') && line.charAt(j + 1) == ' ') {
						line = removeCharAt(line, j + 1);
						j--;
					}
					if ((line.charAt(j + 1) == '{') && line.charAt(j) != ' ') {
						line = insertCharAt(' ', line, j + 1);
						j++;
					}
				}
			}

			// Padding für two-char Operators
			for (int j = 0; j < line.length() - 1; j++) {
				if (isOperator(line.charAt(j), line.charAt(j + 1))) {
					if (line.charAt(j + 2) != ' ')
						line = insertCharAt(' ', line, j + 2);
					if (line.charAt(j - 1) != ' ')
						line = insertCharAt(' ', line, j);
					j += 3;
				}
			}
			rawProgram.set(i, line);
		}

	}

	/** Add correct tabwise indentation. */
	private static void indent() {
		int brack = 0;
		for (int i = 0; i < rawProgram.size(); i++) {
			String s = rawProgram.get(i);
			if (s.indexOf(CB) != -1)
				brack--;
			if (brack < 0)
				throw new IllegalCodeFormatException(i, "There are more closed than open brackets.");
			rawProgram.set(i, "\t".repeat(brack) + s.stripIndent());
			if (s.indexOf(OB) != -1)
				brack++;
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
	 * Checks, if this character is a single-char operator. Doesn't include > and <, because they are
	 * also used for definitions.
	 */
	private static boolean isSingleCharOp(char prev, char op) {
		if (prev == '<' || prev == '>')
			return false;
		return op == '+' || op == '-' || op == '*' || op == '/' || op == '%' || op == '=' || op == '^';
	}

	/**
	 * Checks, if this character is a op-assignment or ==, <=, >=.
	 */
	private static boolean isOperator(char first, char next) {
		if (next == '=')
			return isOperator(' ', first) || first == '<' || first == '>';
		return false;
	}

	/**
	 * Formats {@link OpenBlock} Brackets.
	 * 
	 * Move everything behind a {@link OpenBlock}-Bracket into the next line and connects the bracket to
	 * the statement.
	 * 
	 * <pre>
	 * if false 
	 * 
	 * {print("hi")
	 * 
	 * becomes
	 * 
	 * if false {
	 *     print("hi")
	 * </pre>
	 */
	@InterferesWithStrings
	private static void formatOpenScope() {
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			int idxOfOs = line.indexOf(OB);
			if (idxOfOs != -1 && Helper.isRunnableCode(idxOfOs, line)) {
				// If something is behind the open block, move it down.
				if (idxOfOs != line.length() - 1) {
					rawProgram.add(i + 1, rawProgram.get(i).substring(idxOfOs + 1, line.length()));
					rawProgram.set(i, rawProgram.get(i).substring(0, idxOfOs + 1));
				}
				// If a line starts with an open block...
				if (line.stripIndent().startsWith(OB)) {
					rawProgram.remove(i);
					// Go back and reconnect it to the statement
					for (int j = i - 1; j >= 0; j--) {
						line = rawProgram.get(j);
						if (!line.isBlank() && Helper.isRunnableCode(0, line)) {
							rawProgram.set(j, line + " " + OB);
							break;
						}
					}
				}
			}
		}
	}

	/**
	 * If multiple {@link CloseBlock}-Symbols follow each other, they get seperated and if something
	 * stands in front of a {@link CloseBlock}, that also gets split. This doesn't appl
	 * 
	 * <pre>
	 * 
	 * }} else {...}
	 * 
	 * becomes
	 * 
	 * }
	 * } else {
	 * ...
	 * }
	 * 
	 * </pre>
	 */
	@InterferesWithStrings
	private static void formatClosedScope() {
		for (int i = 0; i < rawProgram.size(); i++) {
			String line = rawProgram.get(i);
			int idxOfCs = line.indexOf(CB);
			if (idxOfCs != -1 && Helper.isRunnableCode(idxOfCs, line)) {
				final String lineWthtFstCS = line.substring(1).stripLeading();
				if (!line.startsWith(CB)) {
					rawProgram.add(i, line.substring(0, idxOfCs));
					rawProgram.set(i + 1, line.substring(idxOfCs));
				}
				// If line starts with multiple OS, possibly seperated by blanks.
				else if (lineWthtFstCS.startsWith(CB)) {
					rawProgram.set(i, CB);
					rawProgram.add(i + 1, lineWthtFstCS);
				}
			}
		}
	}

	/** Move all import statements to the top of the file */
	private static void moveImportsUp() {
		ArrayList<String> imports = new ArrayList<>();
		for (int i = 0; i < rawProgram.size(); i++) {
			if (rawProgram.get(i).startsWith(KeywordType.IMPORT.toString())) {
				imports.add(rawProgram.get(i));
				rawProgram.remove(i);
			}
		}
		Collections.sort(imports, Comparator.reverseOrder());
		for (String imp : imports)
			rawProgram.add(0, imp);
	}

	/**
	 * Sets the order of Flags.
	 * 
	 * Removes flags that are not positioned at the start of the line.
	 * 
	 * Removes unnecessary flags.
	 * 
	 * Removes unnecessary {@value #OLS} in a one-line {@link FlagSpace}.
	 */
	private static void orderFlags() {
		List<String> flags = new ArrayList<>();
		for (int i = 0; i < rawProgram.size(); i++) {
			String[] line = rawProgram.get(i).stripLeading().split(" ");
			// Removes unnecessary {@value #OLS} in a one-line {@link FlagSpace}.
			for (int j = 0; j < line.length; j++) {
				String word = line[j];
				String wordWthtOls = word.replaceAll(String.valueOf(OLS), "");
				if (!isFlag(wordWthtOls))
					break;
				int idxOfOls = word.indexOf(OLS);
				if (idxOfOls != 1 && Helper.isRunnableCode(idxOfOls, rawProgram.get(i)) && isFlag(wordWthtOls))
					line[j] = wordWthtOls;
			}
			// Save flags in List
			flags.addAll(Arrays.stream(line).takeWhile(word -> isFlag(word)).toList());
			if (!flags.isEmpty()) {
				flags = FlagType.orderFlags(flags);
				StringBuilder sb = new StringBuilder();
				for (String flag : flags)
					sb.append(flag + " ");
				String rest = Arrays.stream(line).dropWhile(e -> isFlag(e)).reduce("", (t, e) -> t + " " + e);
				rawProgram.set(i, sb + rest);
				flags.clear();
			}
		}
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

/**
 * Methods that are tagged with this annotation can potentially interfere with strings. They should
 * use the function Formatter#isRunnableCode
 */
@Retention(RetentionPolicy.SOURCE)
@interface InterferesWithStrings {
}