package types.specific;

import expressions.abstractions.Expression;
import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.loops.ConditionalLoop;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.IntervalLoop;
import expressions.main.statements.ConditionalStatement;
import expressions.main.statements.IsStatement;
import expressions.main.statements.ReturnStatement;
import types.AbstractType;
import types.SuperType;

/**
 * Specifies all Keywords and their text-representations. This includes non-functional keywords like
 * include and flags like native.
 *
 */
public enum KeywordType implements AbstractType {

	IF("if"), ELIF("elif"), ANY("any"), ELSE("else"),

	FOR("for"), FROM("from"), REPEAT("repeat"), UNTIL("until"), WHILE("while"),

	FUNC("func"), MAIN("main"), RETURN("return"),

	IMPORT("import"),

	IS("is");

	final String keyword;

	private KeywordType(String keyword) {
		this.keyword = keyword;
	}

	@Override
	public String toString() {
		return keyword;
	}

	@Override
	public Expression create(String arg, int lineID) {
		if (!keyword.equals(arg.strip()))
			return null;
		return switch (this) {
			case FOR -> new ForEachLoop(lineID);
			case FUNC -> new Function(lineID);
			case IS -> new IsStatement(lineID);
			case MAIN -> new MainFunction(lineID);
			case RETURN -> new ReturnStatement(lineID);
			case FROM, REPEAT -> new IntervalLoop(lineID, this);
			case IF, ELIF, ANY, ELSE -> new ConditionalStatement(lineID, this);
			case WHILE, UNTIL -> new ConditionalLoop(lineID, this);
			case IMPORT -> throw new AssertionError("Imports should be filtered out at this point.");
			case null -> null;
		};
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.KEYWORD_TYPE;
	}

	/** Checks, if the passed {@link String} is a {@link KeywordType}. */
	public static boolean isKeyword(String arg) {
		for (KeywordType t : values()) {
			if (t.keyword.equals(arg))
				return true;
		}
		return false;
	}
}