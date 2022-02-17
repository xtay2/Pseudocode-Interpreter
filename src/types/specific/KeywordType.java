package types.specific;

import expressions.abstractions.Expression;
import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.loops.ConditionalLoop;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.loops.RepeatLoop;
import expressions.main.statements.ConditionalStatement;
import expressions.main.statements.IsStatement;
import expressions.main.statements.ReturnStatement;
import types.SpecificType;

/**
 * Specifies all Keywords and their text-representations. This includes
 * non-functional keywords like include and flags like native.
 *
 */
public enum KeywordType implements SpecificType {

	ELIF("elif"), ELSE("else"), FOR("for"), FROM("from"), FUNC("func"), IF("if"), IMPORT("import"), MAIN("main"),
	REPEAT("repeat"), RETURN("return"), UNTIL("until"), WHILE("while"), IS("is");

	public static KeywordType getKeywordTypeFromString(String val) {
		for (KeywordType k : KeywordType.values()) {
			if (k.keyword.equals(val))
				return k;
		}
		return null;
	}

	final String keyword;

	private KeywordType(String keyword) {
		this.keyword = keyword;
	}

	@Override
	public String toString() {
		return keyword;
	}

	/**
	 * Builds an expression from a string or returns null if the string didn't match
	 * any keyword.
	 */
	public static Expression buildKeywordExpressionFromString(String arg, int lineID) {
		KeywordType type = getKeywordTypeFromString(arg);
		if (type == null)
			return null;
		return switch (type) {
		case FOR -> new ForEachLoop(lineID);
		case FROM -> new FromToLoop(lineID);
		case FUNC -> new Function(lineID);
		case IS -> new IsStatement(lineID);
		case MAIN -> new MainFunction(lineID);
		case REPEAT -> new RepeatLoop(lineID);
		case RETURN -> new ReturnStatement(lineID);
		case IF, ELIF, ELSE -> new ConditionalStatement(lineID, type);
		case WHILE, UNTIL -> new ConditionalLoop(lineID, type);
		case IMPORT -> throw new AssertionError("Imports should be filtered out at this point.");
		};
	}
}