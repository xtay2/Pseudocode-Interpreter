package types.specific;

import static types.SuperType.*;

import expressions.abstractions.ScopeHolder;
import expressions.main.functions.Function;
import expressions.main.loops.IntervalLoop;
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
	ARRAY_START("["),

	/** Closed Array-Bracket ] */
	ARRAY_END("]"),

	/** Found in any {@link BracketedExpression}, {@link Call}, {@link Function}, etc... */
	OPEN_BRACKET("("),

	/** Found in any {@link BracketedExpression}, {@link Call}, {@link Function}, etc... */
	CLOSE_BRACKET(")"),

	/** Found in any {@link ScopeHolder} */
	OPEN_SCOPE("{"),

	/** Found in any {@link ScopeHolder} */
	CLOSE_SCOPE("}"),

	/** An Arrow at the end of a func-declaration, used to indicate an oncoming return type. */
	EXPECTED_RETURN_TYPE("->"),

	/** A comma, used in Arrays, Calls, Declarations, etc.. */
	COMMA(","),

	/** Open and Closing Vertical Separator | */
	MULTI_CALL_LINE("|"),

	/** Part of the {@link IntervalLoop}. */
	TO("to"),

	/** Part of the {@link IntervalLoop}. */
	STEP("step");

	public final String id;

	/**
	 * Defines a BuilderType
	 * 
	 * @param id       is the unique identifying symbol from the code.
	 * @param expected are the expected following types. BuilderTypes allways expect themselves as
	 *                 followups.
	 */
	private BuilderType(String id) {
		this.id = id;
	}

	@Override
	public String toString() {
		return id;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.BUILDER_TYPE;
	}

	@Override
	public AbstractType[] expected() {
		return switch (this) {
			case ARRAY_START -> new AbstractType[] { EXPRESSION_TYPE, PREFIX_OPERATOR };
			case ARRAY_END -> new AbstractType[] { KEYWORD_TYPE, ASSIGNMENT_TYPE, INFIX_OPERATOR, POSTFIX_OPERATOR };
			case OPEN_BRACKET -> new AbstractType[] { EXPECTED_TYPE, EXPRESSION_TYPE, ARRAY_START, OPEN_BRACKET };
			case CLOSE_BRACKET -> new AbstractType[] { INFIX_OPERATOR, POSTFIX_OPERATOR, OPEN_SCOPE, EXPECTED_RETURN_TYPE };
			case EXPECTED_RETURN_TYPE -> new AbstractType[] { EXPECTED_TYPE };
			case COMMA -> new AbstractType[] { EXPECTED_TYPE, EXPRESSION_TYPE };
			case MULTI_CALL_LINE -> new AbstractType[] { EXPRESSION_TYPE, POSTFIX_OPERATOR };
			case TO, STEP -> new AbstractType[] { EXPRESSION_TYPE };
			case OPEN_SCOPE, CLOSE_SCOPE -> new AbstractType[] {};
		};
	}
}
