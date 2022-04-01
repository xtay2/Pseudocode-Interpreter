package building.types.specific;

import static building.types.abstractions.SuperType.*;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.main.loops.IntervalLoop;
import building.expressions.normal.BuilderExpression;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.possible.Call;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;

/**
 * Because no {@link BuilderExpression} has its own class, they, and all their
 * expected followers are defined here.
 */
public enum BuilderType implements SpecificType {

	/** Opened Array-Bracket [ */
	ARRAY_START("["),

	/** Closed Array-Bracket ] */
	ARRAY_END("]"),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function},
	 * etc...
	 */
	OPEN_BRACKET("("),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function},
	 * etc...
	 */
	CLOSE_BRACKET(")"),

	/** Found in any {@link ScopeHolder} */
	OPEN_BLOCK("{"),

	/** Found in any {@link ScopeHolder} */
	CLOSE_BLOCK("}"),

	/**
	 * An Arrow at the end of a func-declaration, used to indicate an oncoming
	 * return type.
	 */
	ARROW_R("->"),

	/** A comma, used in Arrays, Calls, Declarations, etc.. */
	COMMA(","),

	/** Open and Closing Vertical Separator | */
	MULTI_CALL_LINE("|"),

	/** Part of the {@link IntervalLoop}. */
	TO("to"),

	/** Part of the {@link IntervalLoop}. */
	STEP("step");

	final String symbol;

	/**
	 * Defines a BuilderType
	 * 
	 * @param id       is the unique identifying symbol from the code.
	 * @param expected are the expected following types. BuilderTypes allways expect
	 *                 themselves as followups.
	 */
	BuilderType(String id) {
		this.symbol = id;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return switch (this) {
			case ARRAY_START -> new AbstractType[] { VAL_HOLDER_TYPE, ARRAY_END };
			case ARRAY_END -> new AbstractType[] { ARRAY_START, AFTER_VALUE_TYPE, ASSIGNMENT_TYPE };
			case OPEN_BRACKET -> new AbstractType[] { VAL_HOLDER_TYPE, CLOSE_BRACKET };
			case CLOSE_BRACKET -> new AbstractType[] { ARRAY_START, OPEN_BRACKET, DYNAMIC_TYPE, AFTER_VALUE_TYPE, ARROW_R };
			case ARROW_R -> new AbstractType[] { DATA_TYPE };
			case COMMA -> new AbstractType[] { VAL_HOLDER_TYPE };
			case MULTI_CALL_LINE -> new AbstractType[] { VAL_HOLDER_TYPE, AFTER_VALUE_TYPE };
			case TO, STEP -> new AbstractType[] { VAL_HOLDER_TYPE };
			case OPEN_BLOCK, CLOSE_BLOCK -> new AbstractType[] {};
		};
	}

	@Override
	public String toString() {
		return symbol;
	}
}
