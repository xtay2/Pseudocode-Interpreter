package building.types.specific;

import static building.types.SuperType.*;
import static building.types.specific.ExpressionType.NAME;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.main.functions.Function;
import building.expressions.main.loops.IntervalLoop;
import building.expressions.normal.BuilderExpression;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.possible.Call;
import building.types.AbstractType;
import building.types.SuperType;

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
	OPEN_BLOCK("{"),

	/** Found in any {@link ScopeHolder} */
	CLOSE_BLOCK("}"),

	/** An Arrow at the end of a func-declaration, used to indicate an oncoming return type. */
	EXPECTED_RETURN_TYPE("->"),

	/** A comma, used in Arrays, Calls, Declarations, etc.. */
	COMMA(","),

	/** Open and Closing Vertical Separator | */
	MULTI_CALL_LINE("|"),

	/** Part of the {@link IntervalLoop}. */
	TO("to"),

	/** Part of the {@link IntervalLoop}. */
	STEP("step"),

	/** Start of a {@link LambdaValue}. */
	LAMBDA_DEF("\\");

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
			case ARRAY_START -> new AbstractType[] { EXPRESSION_TYPE, PREFIX_OPERATOR, ARRAY_END };
			case ARRAY_END -> new AbstractType[] { KEYWORD_TYPE, ASSIGNMENT_TYPE, INFIX_OPERATOR, POSTFIX_OPERATOR, BUILDER_TYPE };
			case OPEN_BRACKET -> new AbstractType[] { EXPECTED_TYPE, EXPRESSION_TYPE, ARRAY_START, OPEN_BRACKET, CLOSE_BRACKET };
			case CLOSE_BRACKET -> new AbstractType[] { ARRAY_START, OPEN_BRACKET, EXPECTED_TYPE, EXPRESSION_TYPE, INFIX_OPERATOR,
					POSTFIX_OPERATOR, OPEN_BLOCK, EXPECTED_RETURN_TYPE, CLOSE_BRACKET };
			case EXPECTED_RETURN_TYPE -> new AbstractType[] { EXPECTED_TYPE };
			case COMMA -> new AbstractType[] { EXPECTED_TYPE, EXPRESSION_TYPE };
			case MULTI_CALL_LINE -> new AbstractType[] { EXPRESSION_TYPE, POSTFIX_OPERATOR };
			case TO, STEP -> new AbstractType[] { EXPRESSION_TYPE };
			case OPEN_BLOCK, CLOSE_BLOCK -> new AbstractType[] {};
			case LAMBDA_DEF -> new AbstractType[] { NAME };
		};
	}
}
