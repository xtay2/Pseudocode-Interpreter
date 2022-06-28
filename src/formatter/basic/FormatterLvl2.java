package formatter.basic;

import static building.types.specific.BuilderType.*;
import static misc.helper.CollectionHelper.*;
import static misc.helper.ProgramHelper.*;
import static misc.util.Regex.*;

import java.util.*;
import java.util.regex.*;

import building.types.abstractions.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import building.types.specific.operators.*;
import misc.helper.*;

/**
 * This gets executed first!
 *
 * <pre>
 * For each line:
 * {@link #reduceSpaces(String, boolean)}
 * {@link #bracketPadding(String, boolean)}
 * {@link #assignmentPadding(String, boolean)}
 * {@link #miscPadding(String, boolean)}
 * {@link #infixOpPadding(String, boolean)}
 * </pre>
 *
 * @see Formatter
 */
public final class FormatterLvl2 extends Formatter {
	
	protected static void format() {
	//@formatter:off
	forEachLine(
		(x, y) -> reduceSpaces(x, y),
		(x, y) -> scopeHolderPadding(x, y),
		(x, y) -> bracketPadding(x, y),
		(x, y) -> assignmentPadding(x, y),
		(x, y) -> miscPadding(x, y),
		(x, y) -> infixOpPadding(x, y)
		);
	//@formatter:on
	}
	
	/**
	 * A {@link LineFormatterFunc} that replaces multiple occurrences of whitespaces with just one.
	 */
	static String reduceSpaces(String line, boolean isFullyRunnable) {
		return replaceAllIfRunnable(line, "\\s{2,}", " ", isFullyRunnable);
	}
	
	/**
	 * A {@link LineFormatterFunc} that add padding between scopes and braces.
	 */
	static String scopeHolderPadding(String line, boolean isFullyRunnable) {
		for (String start : FormatterLvl5.expectedStart)
			line = replaceAllIfRunnable(line, start + "(?=\\()", start + " ", isFullyRunnable);
		return line;
	}
	
	/**
	 * A {@link LineFormatterFunc} that corrects the padding around every occurrence of an
	 * {@link AssignmentType} in the passed line.
	 */
	static String assignmentPadding(String line, boolean isFullyRunnable) {
		
		final SpecificType[] opAssings = Arrays.stream(AssignmentType.values()).filter(e -> e.op != null).toArray(SpecificType[]::new);
		line = typePadding(opAssings, new SpecificType[] {}, line, isFullyRunnable);
		
		//@formatter:off
		SpecificType[] container = {
			InfixOpType.EQUALS 	// = in ==
		}; //@formatter:on
		
		container = CollectionHelper.merge(opAssings, container);
		return typePadding(new SpecificType[] {AssignmentType.NORMAL}, container, line, isFullyRunnable);
	}
	
	/**
	 * A {@link LineFormatterFunc} that corrects the padding around every occurrence of an
	 * {@link InfixOpType} in the passed line.
	 */
	static String infixOpPadding(String line, boolean isFullyRunnable) {
		//@formatter:off
		SpecificType[] container = {
			DynamicType.NAME,		// "and" in candy
			KeywordType.MAIN,		// "in" in main
			PrefixOpType.INC, 		// "+" in ++
			PrefixOpType.DEC, 		// "-" in --
			PrefixOpType.NEG,		// "-" in -6
			BuilderType.ARROW_R,	// "-" in ->
			SingleType.INT, 		// "in" in int
			KeywordType.FOR,		// "or" in for
			KeywordType.IMPORT		// "or" in import
		}; //@formatter:on
		container = merge(container, AssignmentType.values());
		return typePadding(InfixOpType.values(), container, line, isFullyRunnable);
	}
	
	/**
	 * Surrounds every occurrence of any passed type in the line with whitespaces.
	 *
	 * @param types is an array of types that should get padded in this execution.
	 * @param container is an array of types that could contain the target-types in their
	 * {@link String}-representation.
	 * @param line is the line that gets evaluated.
	 * @param isFullyRunnable tells if the line contains comments or strings.
	 * @return the line with optimal possible padding around the types.
	 */
	private static String typePadding(SpecificType[] types, SpecificType[] container, String line, boolean isFullyRunnable) {
		for (SpecificType type : types) {
			final String typeStr = type.toString();
			final String typeQuoted = Pattern.quote(typeStr); // Regex-Escaped opStr
			Pattern p = Pattern.compile("(" + typeQuoted + "(?!\\s))|((?<!\\s)" + typeQuoted + ")");
			Matcher m;
			int lastMatch = 0;
			// Find the first occurrence, fix it, repeat for what left behind the fix.
			while ((m = p.matcher(line)).find(lastMatch)) {
				lastMatch = m.start();
				if (isFullyRunnable || isNotInString(lastMatch, line))
					line = formatNextMatch(line, lastMatch, typeStr, container);
				lastMatch++; // Go on, in case nothing was found.
			}
		}
		return line;
	}
	
	/**
	 * Formats the next matched type with wrong padding.
	 *
	 * @param line is the line thats currently formatted.
	 * @param matchIdx is the index of the current match.
	 * @param typeStr is the string of the operator that was found.
	 * @return the same line but with correct padding around the match.
	 */
	private static String formatNextMatch(String line, int matchIdx, String typeStr, SpecificType[] container) {
		if (matchIdx + typeStr.length() + 1 > line.length())
			return line;
		// Check if the type is contained in another type
		char prev = matchIdx == 0 ? ' ' : line.charAt(matchIdx - 1);
		char next = line.charAt(matchIdx + typeStr.length());
		if (isNotInContainer(typeStr, prev, next, container)) {
			return line.substring(0, matchIdx).stripTrailing() //
					+ " " + typeStr + " " // Correct padding
					+ line.substring(matchIdx + typeStr.length()).stripLeading(); //
		}
		return line;
	}
	
	/**
	 * Checks if the current type is contained in the {@link String}-representation of another type.
	 *
	 * @param current is a validated {@link String}-representation of a {@link SpecificType}.
	 * @param next is the character thats following the type.
	 * @param container is an array of types that could contain this type.
	 * @return true if the passed type isn't part of another one.
	 */
	private static boolean isNotInContainer(String current, char prev, char next, SpecificType[] container) {
		final String mrg = current + next;
		for (SpecificType c : container) {
			if (c == DynamicType.NAME) {
				// If current + next could be a Name
				if (WR.matches(mrg))
					return false;
			}
			// If current is the end of another type, like the second + in ++
			if (c.toString().endsWith(prev + current))
				return false;
			// If current + next is part of another type or current is fully equal.
			if (c.toString().contains(mrg) || c.toString().equals(current))
				return false;
		}
		return true;
	}
	
	/**
	 * A {@link LineFormatterFunc} that corrects the padding for three types of brackets: (), [], {}.
	 *
	 * This should get executed after {@link #reduceSpaces(String, boolean)}
	 */
	static String bracketPadding(String line, boolean isFullyRunnable) {
		// ... in front of open scopes. (Not for functions or array-initializers)
		line = replaceAllIfRunnable(line, "(?<=" + ALPHANUM + ")\\{", " {", isFullyRunnable);
		// ...after open brackets
		line = replaceAllIfRunnable(line, "\\(\\s", "(", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\[\\s", "[", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\{\\s", "{", isFullyRunnable);
		
		// ... in front of closing brackets
		line = replaceAllIfRunnable(line, "\\s\\)", ")", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\s\\]", "]", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\s\\}", "}", isFullyRunnable);
		// ... after closing brackets
		line = replaceAllIfRunnable(line, "\\)(?=" + ALPHANUM + ")", ") ", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\](?=" + ALPHANUM + ")", "] ", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\}(?=" + ALPHANUM + ")", "} ", isFullyRunnable);
		return line;
	}
	
	/**
	 * A {@link LineFormatterFunc} that corrects the padding for commas, colons and semicolons.
	 *
	 * This should get executed after {@link #reduceSpaces(String, boolean)}
	 */
	static String miscPadding(String line, boolean isFullyRunnable) {
		// Arrow
		line = replaceAllIfRunnable(line, "(?<!\\s)" + ARROW_R, " " + ARROW_R, isFullyRunnable);
		line = replaceAllIfRunnable(line, ARROW_R + "(?!\\s)", ARROW_R + " ", isFullyRunnable);
		// Missing following space
		line = replaceAllIfRunnable(line, ",(?=\\S)", ", ", isFullyRunnable);
		line = replaceAllIfRunnable(line, ":(?=\\S)", ": ", isFullyRunnable);
		// Space in front
		line = replaceAllIfRunnable(line, "\\s,", ",", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\s:", ":", isFullyRunnable);
		line = replaceAllIfRunnable(line, "\\s;", ";", isFullyRunnable);
		return line;
	}
}
