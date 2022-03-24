package formatter.basic;

import static misc.helper.Helper.isRunnableCode;
import static misc.helper.Output.print;

import java.util.Collections;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import building.types.specific.BuilderType;
import interpreting.modules.parser.Parser;

public sealed abstract class Formatter permits FormattingPreChecks,FormatterLvl1,FormatterLvl2,FormatterLvl3,FormatterLvl4,FormatterLvl5 {

	static List<String> program;

	/**
	 * Formats a program dependent on the strength-level.
	 * 
	 * @param rawProgram is the unformatted program.
	 * @param level      is the strength of the formatter.
	 * @param isMain     tells, if the formatted file is the Main.pc-file
	 * @return the formatted program.
	 */
	public static final List<String> format(List<String> rawProgram, int level, boolean isMain) {
		if (level < 1 && level > 5)
			throw new IllegalArgumentException("The level of the formatter has to be between 1 and 5. Was: " + level);
		program = rawProgram.stream().map(l -> l.strip()).collect(Collectors.toList()); // Stripping has to occur first
		print("Executing formatting-pre-checks.");
		FormattingPreChecks.check();
		print("Formatting the program on level " + level + ".");
		/////////////////////////////////////////
		// Padding
		if (level >= 2)
			FormatterLvl2.format();
		// Necessary
		if (level >= 1)
			FormatterLvl1.format(isMain);
		/////////////////////////////////////////
		// Shortening
		if (level >= 5)
			FormatterLvl5.format();
		// Styling
		if (level >= 4)
			FormatterLvl4.format();
		/////////////////////////////////////////
		// Redundancy
		if (level >= 3)
			FormatterLvl3.format();
		/////////////////////////////////////////
		indent(); // Indentation comes last
		return program;
	}

	/** Add correct tabwise indentation. */
	static void indent() {
		int brack = 0;
		for (int i = 0; i < program.size(); i++) {
			String s = program.get(i);
			if (s.indexOf(CB) != -1)
				brack--;
			program.set(i, "\t".repeat(brack) + s.stripIndent());
			if (s.indexOf(OB) != -1)
				brack++;
		}
	}

	// STATIC INHERITED

	//@formatter:off
	public static final char
	
	/** The symbol of a one-line-start : */
	OLS = ':';
	
    public static final String

  	/** The symbol of a OpenBlock. { Has to be ecaped in a regex.*/
  	OB = BuilderType.OPEN_BLOCK.toString(),
  	
  	/** The symbol of a CloseBlock. } Has to be ecaped in a regex.*/
  	CB = BuilderType.CLOSE_BLOCK.toString(),
  		
	/** Open Block-Regex: "{" */
  	OBR = "\\" + OB,
  	
	/** Close Block-Regex: "}" */
  	CBR = "\\" + CB,
  	
  	/** 
  	 * The Regex for an open scope, either " {" or ": "
  	 * 
  	 * THIS IS ONLY A LOOK-AHEAD-ATTACHMENT AND SHOULDN'T BE USED ALONE!
  	 */
  	OSR = "((?=" + OLS + "\\s)|(?=\\s" + OBR +"))",
  	
  	/** The symbol of a multi-close-scope ; */
  	MCS = String.valueOf(Parser.MULTI_CLOSE_SCOPE),
  			
  	/** The symbol of a single-line-comment # */
  	SLC = String.valueOf(Parser.SINGLE_LINE_COMMENT);
    
    //@formatter:on

	/**
	 * Replaces all matches of the regex in the line with the replacement, if they
	 * are runnable code.
	 * 
	 * @param line            is the input line that gets tested
	 * @param regex           is the pattern
	 * @param replacement     is the replacement of the matches
	 * @param isFullyRunnable if the line was tested as fully runnable, the
	 *                        {@link String#replaceAll(String, String)} method gets
	 *                        chosen instead.
	 * @return the formatted string
	 */
	static String replaceAllIfRunnable(String line, String regex, String replacement, boolean isFullyRunnable) {
		if (isFullyRunnable)
			return line.replaceAll(regex, replacement);
		Matcher m = Pattern.compile(regex).matcher(line);
		final String unedited = line;
		// Filter out all matches that aren't runnable
		List<MatchResult> matches = m.results().filter(r -> isRunnableCode(r.start(), unedited)).collect(Collectors.toList());
		// Replace all matches, back to front
		Collections.reverse(matches);
		for (MatchResult match : matches)
			line = line.substring(0, match.start()) + replacement + line.substring(match.end());
		return line;
	}

	/**
	 * This function tells, if there is any match of the regex in the line, that is
	 * also runnable.
	 * 
	 * @param line  is the whole line.
	 * @param regex is the regular expression that gets matched.
	 * @return true if the line contains that runnable expression.
	 */
	static boolean containsRunnable(String line, String regex) {
		return Pattern.compile(regex).matcher(line).results().anyMatch(mRes -> isRunnableCode(mRes.start(), line));
	}

	/**
	 * Every formatting-function that only edits one line is a
	 * {@link LineFormatterFunc}. They all get called in
	 * {@link Formatter#forEachLine(List)}.
	 */
	@FunctionalInterface
	interface LineFormatterFunc {
		/**
		 * A method that only edits one line.
		 * 
		 * @param line            the unedited line.
		 * @param isFullyRunnable true if the line contains comments or string-literals.
		 * @return the edited line.
		 */
		String formatLine(String line, boolean isFullyRunnable);
	}

	/**
	 * Executes the formatting that can be done linewise, i.e is not dependent on
	 * other lines.
	 */
	static void forEachLine(List<LineFormatterFunc> functions) {
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i);
			if (line.isBlank() || line.startsWith(SLC))
				continue;
			// If the line contains no comments/strings, repeated checking can be avoided.
			final boolean isFullyRunnable = isFullyRunnable(line);
			for (LineFormatterFunc func : functions)
				line = func.formatLine(line, isFullyRunnable);
			program.set(i, line);
		}
	}

	/** Returns true if the line doesn't contain comments or strings. */
	static boolean isFullyRunnable(String line) {
		return !line.contains(SLC) && !line.contains("\"");
	}

	/**
	 * Comment out multiple lines in the {@link Formatter#program}.
	 * 
	 * @param start is the start-index (inclusive)
	 * @param end   is the end index (inclusive)
	 */
	static void commentRange(int start, int end) {
		for (int i = start; i <= end; i++)
			comment(i);
	}

	/**
	 * Comments out the line at the specified index in {@link Formatter#program}.
	 */
	static void comment(int line) {
		program.set(line, comment(program.get(line)));
	}

	/** Returns the same {@link String} with a prepended SLC. */
	static String comment(String line) {
		return SLC + " " + line.stripLeading();
	}

	static int findEndOfScope(int start) {
		if (program.get(start).endsWith(MCS))
			return start;
		int brack = 0;
		for (int i = start; i < program.size(); i++) {
			String line = program.get(i);
			if (containsRunnable(line, OBR))
				brack++;
			if (containsRunnable(line, CBR)) {
				if (--brack == 0)
					return i;
			}
		}
		throw new AssertionError("Found no matching CloseBlock.");
	}

	/**
	 * DEBUGGING: Prints each line of the current program to the console.
	 */
	static void printProgram() {
		program.forEach(l -> System.out.println(l));
	}
}
