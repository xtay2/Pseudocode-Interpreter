package parsing.program;

import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.loops.RepeatLoop;
import expressions.main.loops.WhileUntilLoop;
import expressions.main.loops.WhileUntilLoop.Type;
import expressions.main.statements.ElifStatement;
import expressions.main.statements.ElseStatement;
import expressions.main.statements.IfStatement;
import expressions.main.statements.IsStatement;
import expressions.main.statements.ReturnStatement;
import expressions.normal.Expression;
import expressions.normal.Flag;
import expressions.normal.Flag.FlagType;
import expressions.normal.operators.InOperator;
import expressions.special.BuilderExpression;

public enum KeywordType {
	ELIF("elif"), ELSE("else"), FOR("for"), FROM("from"), TO("to"), STEP("step"), FUNC("func"), IF("if"), IMPORT("import"), MAIN("main"),
	NATIVE("native"), REPEAT("repeat"), RETURN("return"), UNTIL("until"), WHILE("while"), IS("is");

	public static KeywordType getKeywordTypeFromString(String val) {
		for (KeywordType k : KeywordType.values()) {
			if (k.keyword.equals(val))
				return k;
		}
		return null;
	}

	final String keyword;

	private KeywordType(final String keyword) {
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
		case ELIF -> new ElifStatement(lineID);
		case ELSE -> new ElseStatement(lineID);
		case FOR -> new ForEachLoop(lineID);
		case FROM -> new FromToLoop(lineID);
		case FUNC -> new Function(lineID);
		case IF -> new IfStatement(lineID);
		case IS -> new IsStatement(lineID);
		case MAIN -> new MainFunction(lineID);
		case REPEAT -> new RepeatLoop(lineID);
		case RETURN -> new ReturnStatement(lineID);
		case WHILE -> new WhileUntilLoop(Type.WHILE, lineID);
		case UNTIL -> new WhileUntilLoop(Type.UNTIL, lineID);
		case NATIVE -> new Flag(FlagType.NATIVE, lineID);
		
		case TO, STEP -> BuilderExpression.build(arg);
		
		case IMPORT -> throw new AssertionError("Imports should be filtered out at this point.");
		};
	}
}