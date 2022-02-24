package modules.parser;

import static helper.Output.LINE_BREAK;
import static helper.Output.print;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;
import helper.Helper;
import modules.parser.Parser.LineInfo;

public class Disassembler {

	static final List<Declaration> declarations = new ArrayList<>();

	static List<LineInfo> program;

	static List<LineInfo> disassemble(List<LineInfo> file) {
		program = file;
		// Remove whitespaces
		program = new ArrayList<>(program.stream().map(e -> new LineInfo(e.line().strip(), e.index())).toList());
		// Clear comments
		clearUnwantedLines();
		// Split a one line statement to three lines
		splitOneLiners();
		// Find called used declarations and list them.
		analyse();
		// Whipe out unused lines
		collapse();
		// Remove all remaining empty or fully commented lines.
		clearUnwantedLines();
		print(LINE_BREAK + "Compressed program: " + LINE_BREAK);
		printProgram(true);
		return program;
	}

	private static void analyse() {
		Declaration main = findMain();
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i).line();
			if (Helper.isNotInString(line.indexOf("func"), line)) {
				int end = line.endsWith("{") ? findEndOfScope(i) : i;
				String name = line.substring(line.indexOf("func") + "func".length() + 1, line.indexOf('('));
				declarations.add(new Declaration(name, i, end, countParamsInDeclaration(line), findCallsBetween(i, end)));
			}
		}
		recursive(main);
		print(LINE_BREAK);
		print("All detected Functions: ");
		print(declarations.toString());
		print("Called Functions: ");
		List<Declaration> filtered = declarations.stream().filter(e -> e.getsCalled).toList();
		print(filtered.toString());
	}

	private static void clearUnwantedLines() {
		for (int i = program.size() - 1; i >= 0; i--) {
			LineInfo line = program.get(i);
			if (line == null || line.line().isBlank() || line.line().charAt(0) == Parser.SINGLE_LINE_COMMENT)
				program.remove(i);
		}
	}

	private static void collapse() {
		for (Declaration d : declarations) {
			if (!d.getsCalled) {
				for (int i = d.start; i <= d.end; i++)
					program.set(i, null);
			}
		}
	}

	private static int countParamsInDeclaration(String call) {
		int params = 0;
		for (int i = 0; i < call.length(); i++) {
			if (call.charAt(i) == ',' && Helper.isRunnableCode(i, call))
				params++;
		}
		return params == 0 ? (call.charAt(call.indexOf('(') + 1) == ')' ? 0 : 1) : params + 1;
	}

	/**
	 * Takes every region in a line, that matches a "call" pattern and is not in a text, transforms them
	 * into {@link Call}, puts them in a list, and returns it.
	 */
	private static List<Call> findCalls(String line) {
		Matcher m = Pattern.compile("\\w+\\(").matcher(line);
		List<String> textCalls = new ArrayList<>();
		List<Call> calls = new ArrayList<>();
		m.results().filter(r -> Helper.isNotInString(r.start(), line)).forEach((e) -> textCalls.add(e.group()));
		textCalls.forEach((e) -> {
			int brack = 1, args = 0, arr = 0;
			boolean inMultiline = false;
			for (int i = line.indexOf(e) + e.length(); i < line.length(); i++) {
				if (Helper.isRunnableCode(i, line)) {
					if (line.charAt(i) == '|')
						inMultiline = !inMultiline;
					else if (!inMultiline) {
						if (line.charAt(i) == '[')
							arr++;
						if (line.charAt(i) == ']')
							arr--;
						if (brack == 1 && arr == 0 && line.charAt(i) == ',')
							args++;
						if (line.charAt(i) == '(')
							brack++;
						if (line.charAt(i) == ')')
							brack--;
						if (brack == 0) {
							if (line.charAt(line.indexOf(e) + e.length()) != ')')
								args++;
							break;
						}
					}
				}
			}
			calls.add(new Call(e.substring(0, e.length() - 1), args));
		});
		return calls;
	}

	/**
	 * Find all calls between two lines ie {@link OpenScope} and {@link CloseScope} of a
	 * {@link Declaration} and returns them as a List of {@link Call}.
	 */
	private static List<Call> findCallsBetween(int start, int end) {
		List<Call> calls = new ArrayList<>();
		for (int i = start; i <= end; i++) {
			String line = program.get(i).line();
			int funcKeyword = line.indexOf("func");
			// Wenn es einem call pattern matched und in der zeile nicht das wort func
			// steht, oder wenn doch in einem string.
			if (line.matches(".*\\w+\\(.*\\);?.*") && (funcKeyword == -1 || !Helper.isRunnableCode(funcKeyword, line)))
				calls.addAll(findCalls(line));
		}
		return calls;
	}

	private static int findEndOfScope(int start) {
		int brack = 0;
		for (int i = start; i < program.size(); i++) {
			String line = program.get(i).line();
			if (line.endsWith("{"))
				brack++;
			if (line.startsWith("}"))
				brack--;
			if (brack == 0)
				return i;
		}
		throw new AssertionError("Has to be called on a valid scope. Was " + program.get(start));
	}

	private static Declaration findMain() {
		for (int i = 0; i < program.size(); i++) {
			if (program.get(i).line().startsWith("main")) {
				int end = findEndOfScope(i);
				return new Declaration("main", i, end, 0, findCallsBetween(i, end));
			}
		}
		throw new AssertionError("Program has to contain a main.");
	}

	private static void printProgram(boolean showLineNrs) {
		program.forEach(e -> print(e.index() == -1 ? e.line() : e.index() + ": " + e.line()));
	}

	private static void recursive(Declaration current) {
		current.getsCalled = true;
		for (Call call : current.calls) {
			try {
				Declaration d = declarations.stream().filter(e -> (e.name + e.arguments)//
						.equals(call.name + call.arguments))//
						.findFirst()//
						.get();
				if (!d.getsCalled) {
					print(current + "\t is calling " + d);
					recursive(d);
				}
			} catch (NoSuchElementException e) {
				throw new AssertionError("Trying to call a function \"" + call.name + "\" with " + call.arguments
						+ " arguments that doesn't get defined or imported.");
			}
		}
	}

	/**
	 * Splits a one-line-statement and removes the semicolon.
	 */
	private static void splitOneLiners() {
		for (int i = 0; i < program.size(); i++) {
			String content = program.get(i).line();
			int index = program.get(i).index();

			int lineBreak = content.indexOf(":");
			if (lineBreak != -1 && Helper.isRunnableCode(lineBreak, content)) {
				if (lineBreak == content.length() - 1)
					throw new IllegalCodeFormatException(program.get(i).index(), "This one-line statement has to end with a semicolon.");
				// Replace Semikolon with ScopeBrackets
				if (content.endsWith(";")) // For Nested Loops/Statements
					content = content.substring(0, content.length() - 1);
				program.add(i + 1, new LineInfo("}", index));
				program.add(i + 1, new LineInfo(content.substring(lineBreak + 2), index)); // Teil nach :
				program.set(i, new LineInfo(content.substring(0, lineBreak) + " {", index));
			}
		}
	}
}
