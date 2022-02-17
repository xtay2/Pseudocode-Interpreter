package types.specific;

import static types.ExpressionType.*;

import java.util.stream.Stream;

import expressions.main.functions.Function;
import expressions.main.loops.FromToLoop;
import expressions.normal.BuilderExpression;
import expressions.normal.brackets.BracketedExpression;
import expressions.possible.Call;
import types.AbstractType;
import types.SpecificType;

/**
 * Because no {@link BuilderExpression} has its own class, they, and all their
 * expected followers are defined here.
 */
public enum BuilderType implements SpecificType {

	/** Opened Array-Bracket [ */
	ARRAY_START("[", LITERAL, NAME, CREMENT),

	/** Closed Array-Bracket ] */
	ARRAY_END("]", KEYWORD, ASSIGNMENT, NAME, INFIX_OPERATOR, OPEN_SCOPE, CREMENT),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function},
	 * etc...
	 */
	OPEN_BRACKET("(", LITERAL, DATA_TYPE, NAME, ARRAY_START, CREMENT),

	/**
	 * Found in any {@link BracketedExpression}, {@link Call}, {@link Function},
	 * etc...
	 */
	CLOSE_BRACKET(")", OPEN_SCOPE, INFIX_OPERATOR),

	/**
	 * An Arrow at the end of a func-declaration, used to indicate an oncoming
	 * return type.
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
	 * @param expected are the expected following types. BuilderTypes allways expect
	 *                 themselves as followups.
	 */
	private BuilderType(String id, AbstractType... expected) {
		this.id = id;
		this.expected = Stream.of(new AbstractType[] { BUILDER_TYPE }, expected).flatMap(Stream::of).toArray(AbstractType[]::new);
		for (AbstractType t : expected) {
			if (t instanceof SpecificType)
				throw new AssertionError("Specifying " + t + " as an expected followup for " + this
						+ " is redundant because all BuilderTypes already expect following BuilderTypes in general.");
		}
	}

	@Override
	public String toString() {
		return id;
	}
}
