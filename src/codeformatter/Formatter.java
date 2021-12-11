package codeformatter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class Formatter {

	static String[] rawProgram;

	public static String[] format(String[] p) {
		rawProgram = p;
		stripTrailing();
		lineBreakBetweenBlocks(0);
		convertOneLiners();
		ident();
		moveImportsUp();
		correctSpaces();
		addMissingMain();
		return rawProgram;
	}

	private static void stripTrailing() {
		for (int i = 0; i < rawProgram.length; i++) {
			rawProgram[i] = rawProgram[i].stripTrailing();
		}
	}

	private static void lineBreakBetweenBlocks(int start) {
		for (int i = start; i < rawProgram.length; i++) {
			String line = rawProgram[i];
			if (line.chars().filter(ch -> ch == '}').count() > 1 || (line.contains("}") && line.stripIndent().length() > 1)) {
				rawProgram[i] = rawProgram[i].replaceFirst("}", "");
				addLine(i + 1);
				rawProgram[i + 1] = "}";
				lineBreakBetweenBlocks(i > 2 ? i - 2 : 0);
				return;
			}
			int firstOBr = line.indexOf("{");
			if (firstOBr != -1 && firstOBr != line.length() - 1) {
				addLine(i + 1);
				rawProgram[i + 1] = rawProgram[i].substring(firstOBr + 1, line.length());
				rawProgram[i] = rawProgram[i].substring(0, firstOBr + 1);
			}
			if (!line.isBlank() && line.stripIndent().charAt(0) == '{') {
				rawProgram[i - 1] += " {";
				deleteLine(i);
			}
		}
	}

	private static void ident() {
		int brack = 0;
		for (int i = 0; i < rawProgram.length; i++) {
			String s = rawProgram[i];
			if (i >= 2 ? rawProgram[i - 2].indexOf(':') != -1 : false)
				brack--;
			if (s.indexOf('}') != -1)
				brack--;
			rawProgram[i] = "\t".repeat(brack) + s.stripIndent();
			if (s.indexOf('{') != -1 || s.indexOf(':') != -1)
				brack++;
		}
	}

	private static void moveImportsUp() {
		ArrayList<String> imports = new ArrayList<>();
		for (int i = 0; i < rawProgram.length; i++) {
			if(rawProgram[i].startsWith("import")) {
				imports.add(rawProgram[i]);
				deleteLine(i);
			}
		}
		Collections.sort(imports, Comparator.reverseOrder());
		for(String imp : imports) {
			addLine(0);
			rawProgram[0] = imp;
		}
	}

	private static void convertOneLiners() {
		for (int i = 0; i < rawProgram.length - 2; i++) {
			if (!rawProgram[i].isEmpty() && !rawProgram[i + 2].isEmpty()) {
				char thisC = rawProgram[i].charAt(rawProgram[i].length() - 1);
				char nextC = rawProgram[i + 2].charAt(rawProgram[i + 2].length() - 1);
				if (thisC == '{' && nextC == '}') {
					rawProgram[i] = rawProgram[i].substring(0, rawProgram[i].length() - 2) + ":";
					deleteLine(i + 2);
				}
			}
		}
	}

	private static void correctSpaces() {
		for (int i = 0; i < rawProgram.length; i++) {
			String line = rawProgram[i];

			// Entferne alle mehrfachen spaces.
			while (line.contains("  "))
				line = line.replaceAll("  ", " ");

			// Entferne alle spaces vor kommatas.
			for (int j = 1; j < line.length(); j++) {
				if (line.charAt(j) == ',' && line.charAt(j - 1) == ' ')
					line = removeCharAt(line, j - 1);
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
			rawProgram[i] = line;
		}

	}

	private static boolean isArithmeticOperator(char op, char lastChar, char nextChar) {
		if (op == '-' && nextChar != ' ')
			return false;
		return op == '+' || op == '-' || op == '*' || op == '/' || op == '%';
	}

	private static void addMissingMain() {
		for (String line : rawProgram) {
			if (line.stripIndent().startsWith("main"))
				return;
		}
		String[] newProg = new String[rawProgram.length + 4];
		for (int i = 0; i < rawProgram.length; i++)
			newProg[i + 4] = rawProgram[i];
		rawProgram = newProg;
		rawProgram[0] = "#Don't forget your main!";
		rawProgram[1] = "main:";
		rawProgram[2] = "\texit()";
		rawProgram[3] = "";
	}

	private static void addLine(int index) {
		String[] newProg = new String[rawProgram.length + 1];
		for (int i = 0; i < rawProgram.length; i++)
			newProg[i + (index <= i ? 1 : 0)] = rawProgram[i];
		newProg[index] = "";
		rawProgram = newProg;
	}

	private static void deleteLine(int index) {
		String[] newProg = new String[rawProgram.length - 1];
		for (int i = 0; i < newProg.length; i++)
			newProg[i] = rawProgram[i + (index <= i ? 1 : 0)];
		rawProgram = newProg;
	}

	private static String insertCharAt(char c, String s, int i) {
		return s.substring(0, i) + c + s.substring(i);
	}

	private static String removeCharAt(String s, int i) {
		return s.substring(0, i) + s.substring(i + 1);
	}

}
