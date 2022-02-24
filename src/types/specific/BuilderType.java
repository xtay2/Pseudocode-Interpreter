package types.specific;

import static types.SuperType.*;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;

import java.util.Arrays;
import java.util.stream.Stream;

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
	ARRAY_START("[", LITERAL, NAME, POSTFIX_OPERATOR),

	/** Closed Array-Bracket ] */
	ARRAY_END("]", KEYWORD_TYPE, ASSIGNMENT_TYPE, NAME, INFIX_OPERATOR, OPEN_SCOPE, POSTFIX_OPERATOR),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function}, etc...
	 */
	OPEN_BRACKET("(", LITERAL, EXPECTED_TYPE, NAME, ARRAY_START, POSTFIX_OPERATOR),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function}, etc...
	 */
	CLOSE_BRACKET(")", OPEN_SCOPE, INFIX_OPERATOR),

	/**
	 * An Arrow at the end of a func-declaration, used to indicate an oncoming return type.
	 */
	EXPECTED_RETURN_TYPE("->", EXPECTED_TYPE),

	/** A comma, used in Arrays, Calls, Declarations, etc.. */
	COMMA(",", EXPECTED_TYPE, LITERAL, NAME),

	/** Open and Closing Vertical Separator | */
	MULTI_CALL_LINE("|", NAME, POSTFIX_OPERATOR, LITERAL),

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
		this.expected = Stream.concat(Arrays.stream(expected), Arrays.stream(new AbstractType[] { BUILDER_TYPE }))
				.toArray(AbstractType[]::new);
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
