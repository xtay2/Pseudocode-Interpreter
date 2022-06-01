package formatter.basic;

import static misc.helper.ProgramHelper.containsRunnable;
import static misc.supporting.Output.print;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import building.types.specific.BuilderType;
import formatter.FileDataManagement;
import importing.filedata.paths.FilePath;
import launching.Main;
import misc.constants.GreekSymbol;
import misc.supporting.FileManager;

public sealed abstract class Formatter permits FormattingPreChecks, FormatterLvl1, FormatterLvl2, FormatterLvl3, FormatterLvl4, FormatterLvl5 {

	static List<String> program;
	static FilePath filePath;
	static final int level = Main.getFormattingLvl();

	/**
	 * Format all unformatted files.
	 */
	public static final void formatAll(boolean forceFormat) {
		assert (level >= 1 && level <= 5) : "The level of the formatter has to be between 1 and 5. Was: " + level;
		for (FilePath fp : FileDataManagement.getUnformattedFiles(forceFormat)) {
			try {
				Path p = Path.of(fp.getAbsPath());
				FileManager.writeFile(formatFile(Files.readAllLines(p), fp), p);
			} catch (IOException e) {
				throw new AssertionError("Couldn't access previously available file " + fp + ".", e);
			}
		}
	}

	/**
	 * Formats a {@link File} dependent on the strength-level.
	 *
	 * @param rawProgram is the unformatted program.
	 * @param level is the strength of the formatter.
	 * @param isMain tells, if the formatted file is the Main.pc-file
	 * @return the formatted program.
	 */
	private static final List<String> formatFile(List<String> rawProgram, FilePath dataPath) {
		Formatter.filePath = dataPath;
		program = rawProgram.stream().map(l -> l.strip()).collect(Collectors.toList()); // Stripping has to occur first
		print("Executing formatting-pre-checks.");
		FormattingPreChecks.check();
		print("Formatting the program on level " + level + ".");
		/////////////////////////////////////////
		FormatterLvl1.preFormatting();
		// Padding
		if (level >= 2)
			FormatterLvl2.format();
		// Necessary
		FormatterLvl1.format(dataPath.getName().equals(importing.filedata.File.MAIN_FILE));
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
			if (containsRunnable(s, CBR))
				brack--;
			if (brack == -1)
				throw new AssertionError("This shouldn't happen. Line " + i + " is broken:\n" + s);
			program.set(i, "\t".repeat(brack) + s.stripIndent());
			if (containsRunnable(s, OBR))
				brack++;
		}
	}

	// STATIC INHERITED

	//@formatter:off
	public static final char

	/** The symbol of a one-line-start : */
	OLS = ':';

    public static final String

    /** Lowercase Regex ASCII*/
    LCR = "[a-z]",

	/** Uppercase Regex ASCII*/
	UCR = "[A-Z]",

	/** Greek lowercase Regex */
	GLCR = "[" + GreekSymbol.ALPHA.lower+ "-" + GreekSymbol.OMEGA.lower + "]",

	/** Greek uppercase Regex */
	GUCR = "[" + GreekSymbol.ALPHA.upper+ "-" + GreekSymbol.OMEGA.upper + "]",

	/** Umlaut Regex */
    UMLAUTE = "[äöüÄÖÜß]",

	/** ASCII chars, Greek chars, umlaut */
    ALBHABETICAL = LCR + "|" + UCR + "|" + GLCR + "|" + GUCR + "|" + UMLAUTE,

	/** Alphabetical, digits and underscore */
    ALPHANUM = "("+ ALBHABETICAL + "|\\d|_)",

    /** Regex for a word with any length, that contains atleast one {@link #ALPHABETICAL} char. */
    WR = ALPHANUM + "*(" + ALBHABETICAL + ")+" + ALPHANUM + "*",

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
  	MCS = BuilderType.MULTI_CLOSE_SCOPE.toString(),

  	/** The symbol of a single-line-comment # */
  	SLC = BuilderType.SINGLE_LINE_COMMENT.toString();

    //@formatter:on

	/**
	 * Every formatting-function that only edits one line is a {@link LineFormatterFunc}. They all get
	 * called in {@link Formatter#forEachLine(List)}.
	 */
	@FunctionalInterface
	interface LineFormatterFunc {
		/**
		 * A method that only edits one line.
		 *
		 * @param line the unedited line.
		 * @param isFullyRunnable true if the line contains comments or string-literals.
		 * @return the edited line.
		 */
		String formatLine(String line, boolean isFullyRunnable);
	}

	/** Executes the formatting that can be done linewise, i.e is not dependent on other lines. */
	static void forEachLine(LineFormatterFunc... functions) {
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
	public static boolean isFullyRunnable(String line) {
		return !line.contains(SLC) && !line.contains("\"");
	}

	/**
	 * Comment out all uncommented lines between two indices in the {@link Formatter#program}.
	 *
	 * @param start is the start-index (inclusive)
	 * @param end is the end index (inclusive)
	 */
	static void commentRange(int start, int end) {
		for (int i = start; i <= end; i++)
			comment(i);
	}

	/**
	 * Comments out the line at the specified index in {@link Formatter#program}, if its not already
	 * done.
	 */
	static void comment(int line) {
		program.set(line, comment(program.get(line)));
	}

	/** Returns the same {@link String} with a prepended SLC if it doesn't already start with one. */
	static String comment(String line) {
		if (!line.startsWith(SLC) && !line.isBlank())
			return SLC + " " + line.stripLeading();
		return line;
	}
}
