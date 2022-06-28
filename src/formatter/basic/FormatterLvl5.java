package formatter.basic;

import static building.types.specific.FlagType.*;
import static building.types.specific.KeywordType.*;
import static building.types.specific.datatypes.SingleType.*;
import static misc.helper.ProgramHelper.*;
import static misc.helper.StringHelper.*;
import static runtime.datatypes.BoolValue.*;

import java.util.*;

import building.expressions.abstractions.interfaces.*;
import building.types.specific.*;

/**
 * Everything should get executed after {@link FormatterLvl2#format()}.
 * 
 * <pre>
 * For the whole file:
 * {@link #commentDeadCode()}
 * {@link #commentDeadVars()}
 * {@link #commentDeadDefs()}
 * {@link #commentAfterReturn()}
 * For each line:
 * {@link #removeBrackets(String, boolean)}
 * {@link #simplifyBools(String, boolean)}
 * {@link #simplifyImmutables(String, boolean)}
 * </pre>
 *
 * @see Formatter
 */
public final class FormatterLvl5 extends Formatter {
	
	protected static void format() {
		commentDeadScopes();
		commentDeadVars();
		commentDeadDefs();
		commentAfterReturn();
		//@formatter:off
		forEachLine(
			(x, y) -> removeDoubleBrackets(x, y),
			(x, y) -> removeOuterBrackets(x, y),
//			(x, y) -> simplifyBools(x, y),
			(x, y) -> simplifyImmutables(x, y)
			);
		//@formatter:on
	}
	
	/**
	 * Comments out each of the following dead conditionals:
	 * 
	 * <pre>
	 * -elif false
	 * -any if false
	 * -while false
	 * -until true
	 * -repeat 0
	 * </pre>
	 * 
	 * (This doesn't include the "if false"-statement, as it could be followed by another conditional
	 * and thereby have an impact on the behavior.)
	 */
	static void commentDeadScopes() {
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i);
			//@formatter:off
			if(
				containsRunnable(line, ELIF + " " + FALSE + OSR) ||
				containsRunnable(line, ANY + " " + IF + " " + FALSE + OSR) ||
				containsRunnable(line, WHILE + " " + FALSE + OSR) ||
				containsRunnable(line, UNTIL + " " + TRUE + OSR) ||
				containsRunnable(line, REPEAT + " 0" + OSR)
			) { //@formatter:on
				int idxOfCB = findMatchingBrack(program, i, indexOfRunnable(line, OBR))[0];
				if (containsRunnable(line, CBR)) {
					splitCBLineStart(i);
					splitCBLineStart(idxOfCB + 1);
					continue;
				}
				commentRange(i, idxOfCB);
			}
		}
	}
	
	/** Comments out all variable-declarations that dont get used. */
	static void commentDeadVars() {
		// TODO Auto-generated method stub
	}
	
	/** Comments out all definition-declarations that dont get called. */
	static void commentDeadDefs() {
		// TODO Auto-generated method stub
	}
	
	/**
	 * Splits a line that starts with a CB and something behind that into two lines.
	 * 
	 * <pre>
	 * } else
	 * becomes
	 * }
	 * else
	 * </pre>
	 * 
	 * @param lineIdx is the index of the line in {@link Formatter#program}
	 */
	private static void splitCBLineStart(int lineIdx) {
		String line = program.get(lineIdx);
		if (line.length() > 1 && line.startsWith(CB)) {
			program.set(lineIdx, line.substring(0, 1));
			program.add(lineIdx + 1, line.substring(2));
		}
	}
	
	/** Comments out dead code after a return-statement. */
	static void commentAfterReturn() {
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i);
			if (containsRunnable(line, RETURN.toString())) {
				if (lineEndsWith(line, MCS))
					continue;
				// For all lines between return and next CloseBlock
				for (int j = i + 1; j < program.size(); j++) {
					String lnAfterRet = program.get(j);
					if (containsRunnable(lnAfterRet, CBR)) {
						commentRange(i + 1, j - 1);
						i = j + 1;
						break;
					}
				}
			}
		}
	}
	
	/** A {@link LineFormatterFunc} that removes multiple brackets that enclose the same thing. */
	static String removeDoubleBrackets(String line, boolean isFullyRunnable) {
		for (int i = 0; i < line.length(); i++) {
			char c = line.charAt(i);
			if (c == '(') {
				int match = findMatchingBrackInLine(i, line, isFullyRunnable);
				while (line.charAt(i + 1) == '(') {
					int nextMatch = findMatchingBrackInLine(i + 1, line, isFullyRunnable);
					if (match - 1 == nextMatch) {
						line = removeCharAt(nextMatch, line);
						line = removeCharAt(i + 1, line);
						match--;
					} else
						break;
				}
			}
		}
		return line;
	}
	
	/**
	 * List of expression that expect a {@link ValueHolder}.
	 * 
	 * <pre>
	 * Gets used in: 
	 * {@link #removeOuterBrackets(String, boolean)}
	 * {@link FormatterLvl2#scopeHolderPadding(String, boolean)}
	 * </pre>
	 */
	static List<String> expectedStart = List.of(//
			IF.toString(), ELIF.toString(), ANY + " " + IF, WHILE.toString(), UNTIL.toString(), REPEAT.toString());
	
	/**
	 * A {@link LineFormatterFunc} that removes all brackets that enclose the whole expression.
	 * 
	 * <pre>
	 * exp (...) { 	-> exp a + b {
	 * exp (...): 	-> exp a + b:
	 * </pre>
	 * 
	 * TODO: Make this work for any kind of {@link AssignmentType}
	 * 
	 * @see #expectedStart
	 */
	static String removeOuterBrackets(String line, boolean isFullyRunnable) {
		for (String start : expectedStart) {
			if (containsRunnable(line, start + " \\(.+\\)" + OSR)) {
				int idxOfFstBrack = line.indexOf(start + " (") + start.length() + 1;
				if (isRunnableCode(idxOfFstBrack, line)) {
					int idxOfMatch = findMatchingBrackInLine(idxOfFstBrack, line, isFullyRunnable);
					// Check if matching bracket is at the end of the declaration
					if (line.substring(idxOfMatch, idxOfMatch + 3).matches("\\)" + OLS + "\\s|\\)\\s" + OBR)) {
						line = removeCharAt(idxOfMatch, line);
						line = removeCharAt(idxOfFstBrack, line);
					}
				}
			}
		}
		return line;
	}
	
	/**
	 * A {@link LineFormatterFunc} that aggressively removes redundant parts from boolean-expressions.
	 */
	static String simplifyBools(String line, boolean isFullyRunnable) {
		// TODO Implement me!
		return null;
	}
	
	/**
	 * A {@link LineFormatterFunc} that replaces:
	 * 
	 * <pre>
	 * -const var with const 
	 * -final var with final
	 * </pre>
	 */
	static String simplifyImmutables(String line, boolean isFullyRunnable) {
		line = replaceAllIfRunnable(line, CONSTANT + " " + VAR, line, isFullyRunnable);
		line = replaceAllIfRunnable(line, FINAL + " " + VAR, line, isFullyRunnable);
		return line;
	}
}
