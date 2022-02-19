package types.specific;

import static types.SuperType.DATA_TYPE;
import static types.SuperType.KEYWORD_TYPE;
import static types.specific.ExpressionType.ASSIGNMENT;
import static types.specific.ExpressionType.DECREMENT;
import static types.specific.ExpressionType.INCREMENT;
import static types.specific.ExpressionType.INFIX_OPERATOR;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;

import expressions.abstractions.Expression;
import expressions.main.functions.Function;
import expressions.main.loops.FromToLoop;
import expressions.normal.BuilderExpression;
import expressions.normal.brackets.BracketedExpression;
import expressions.possible.Call;
import types.AbstractType;
import types.SuperType;

/**
 * Because no {@link BuilderExpression} has its own class, they, and all their expected followers
 * are defined here.
 */
public enum BuilderType implements AbstractType {

	/** Opened Array-Bracket [ */
	ARRAY_START("[", LITERAL, NAME, INCREMENT, DECREMENT),

	/** Closed Array-Bracket ] */
	ARRAY_END("]", KEYWORD_TYPE, ASSIGNMENT, NAME, INFIX_OPERATOR, OPEN_SCOPE, INCREMENT, DECREMENT),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function}, etc...
	 */
	OPEN_BRACKET("(", LITERAL, DATA_TYPE, NAME, ARRAY_START, INCREMENT, DECREMENT),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function}, etc...
	 */
	CLOSE_BRACKET(")", OPEN_SCOPE, INFIX_OPERATOR),

	/**
	 * An Arrow at the end of a func-declaration, used to indicate an oncoming return type.
	 */
	EXPECTED_RETURN_TYPE("->", DATA_TYPE),

	/** A comma, used in Arrays, Calls, Declarations, etc.. */
	COMMA(",", DATA_TYPE, LITERAL, NAME),

	/** Open and Closing Vertical Separator | */
	MULTI_CALL_LINE("|", NAME),

	/** Part of the {@link FromToLoop}. */
	TO("to", LITERAL, NAME),

	/** Part of the {@link FromToLoop}. */
	STEP("step", LITERAL, NAME);

	public final String id;
	public final AbstractType[] expected;

	/**
	 * Defines a BuilderType
	 * 
	 * @param id       is the unique identifying symbol from the code.
	 * @param expected are the expected following types. BuilderTypes allways expect themselves as
	 *                 followups.
	 */
	private BuilderType(String id, AbstractType... expected) {
		this.id = id;
		this.expected = expected;
	}

	@Override
	public String toString() {
		return id;
	}

	@Override
	public Expression create(String arg, int lineID) {
		if (!id.equals(arg.strip()))
			return null;
		return new BuilderExpression(this);
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.BUILDER_TYPE;
	}
}
