package formatter.basic;

import static building.types.specific.BuilderType.*;
import static building.types.specific.KeywordType.*;
import static building.types.specific.operators.InfixOpType.*;
import static building.types.specific.operators.PrefixOpType.*;
import static misc.helper.ProgramHelper.*;
import static runtime.datatypes.BoolValue.*;
import static runtime.datatypes.numerical.ConceptualNrValue.*;

import java.util.*;

/**
 * Everything should get executed after {@link FormatterLvl2#format()} and {@link FormatterLvl5}, if
 * activated.
 *
 * <pre>
 * For each line:
 * {@link #simplifyInfLoops(String, boolean)}
 * {@link #swapCondLoops(String, boolean)}
 * {@link #shortenBools(String, boolean)}
 * {@link #mergeStringLiterals(String, boolean)}
 * </pre>
 *
 * @see Formatter
 */
public final class FormatterLvl4 extends Formatter {
	
	protected static void format() {
	//@formatter:off
	forEachLine(
		(x, y) -> shortenBools(x, y),
		(x, y) -> simplifyMisc(x, y),
		(x, y) -> swapCondLoops(x, y),
		(x, y) -> simplifyInfLoops(x, y)
		);
	//@formatter:on
	}
	
	/** A {@link LineFormatterFunc} that simplifies/removes redundant terms. */
	static String simplifyMisc(String line, boolean isFullyRunnable) {
		// Remove step 1
		line = replaceAllIfRunnable(line, " " + STEP + " 1", "", isFullyRunnable);
		// Replace any if true with just any
		line = replaceAllIfRunnable(line, ANY + " " + IF + " " + TRUE, ANY.toString(), isFullyRunnable);
		return line;
	}
	
	/**
	 * A {@link LineFormatterFunc} that removes redundant boolean literals/operators.
	 */
	static String shortenBools(String line, boolean isFullyRunnable) {
		int lineL = line.length();
		line = replaceAllIfRunnable(line, NOT + " " + FALSE, TRUE.toString(), isFullyRunnable);
		line = replaceAllIfRunnable(line, NOT + " " + TRUE, FALSE.toString(), isFullyRunnable);
		
		//All bool-expressions that are redundant and can be deleted @formatter:off
		line = List.of(
			NOT + " " + NOT,

			NOT_EQUALS + " " + FALSE,
			FALSE + " " + NOT_EQUALS,

			EQUALS + " " + TRUE,
			TRUE + " " + EQUALS,

			AND + " " + TRUE,
			TRUE + " " + AND,

			OR + " " + FALSE,
			FALSE + " " + OR,

			XOR + " " + FALSE,
			FALSE + " " + XOR
		).stream().reduce(line, (l, exp) -> replaceAllIfRunnable(l, exp, "", isFullyRunnable));
		//@formatter:on
		// Correct the padding, if something has changed.
		if (line.length() < lineL) {
			line = FormatterLvl2.reduceSpaces(line, isFullyRunnable);
			line = FormatterLvl2.bracketPadding(line, isFullyRunnable);
		}
		return line;
	}
	
	/**
	 * A {@link LineFormatterFunc} that simplifies endless loops with "repeat {".
	 *
	 * Should get executed after {@link #shortenBools(String, boolean)}
	 */
	static String simplifyInfLoops(String line, boolean isFullyRunnable) {
		// Replace infinite loops with "repeat {" or "repeat: "
		// repeat INF {
		line = replaceAllIfRunnable(line, REPEAT + " " + POS_INF + OSR, REPEAT.toString(), isFullyRunnable);
		// while true {
		line = replaceAllIfRunnable(line, WHILE + " " + TRUE + OSR, REPEAT.toString(), isFullyRunnable);
		// until false {
		line = replaceAllIfRunnable(line, UNTIL + " " + FALSE + OSR, REPEAT.toString(), isFullyRunnable);
		// from 0 to INF {
		line = replaceAllIfRunnable(line, FROM + " 0 " + TO + POS_INF + OSR, REPEAT.toString(), isFullyRunnable);
		return line;
	}
	
	/**
	 * A {@link LineFormatterFunc} that swaps "while not" with "until", and "until not" with "while".
	 */
	static String swapCondLoops(String line, boolean isFullyRunnable) {
		line = replaceAllIfRunnable(line, WHILE + " " + NOT, UNTIL.toString(), isFullyRunnable);
		line = replaceAllIfRunnable(line, UNTIL + " " + NOT, WHILE.toString(), isFullyRunnable);
		return line;
	}
	
}
