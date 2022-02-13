package expressions.special;

import static parsing.program.ExpressionType.*;

import java.util.List;

import expressions.abstractions.Expression;
import expressions.abstractions.MergedExpression;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;

/**
 * An {@link Expression} that later gets destructed and hasn't any
 * persistency/logic in the code. This acts solely as a part of any
 * {@link MergedExpression}.
 * 
 * @see MergedExpression
 * @see Expression
 */
public class BuilderExpression extends Expression {

	private static final List<ExpressionType> BUILDER_EXPRESSION_TYPES //
			= List.of(ARRAY_START, ARRAY_END, MULTI_CALL_LINE, COMMA, EXPECTED_RETURN_TYPE, OPEN_BRACKET,
					CLOSE_BRACKET);

	private BuilderExpression(ExpressionType myType) {
		super(-1, myType);
		switchExpectedExpressions();
	}

	public BuilderExpression(KeywordType myKeyword) {
		super(-1, myKeyword);
		switchExpectedExpressions();
	}

	private void switchExpectedExpressions() {
		if (myType == KEYWORD) {
			setExpectedExpressions(switch (myKeyword) {
			case NATIVE -> new ExpressionType[] { KEYWORD };

			// The "to"-keyword, used in the FromToLoop
			case TO -> new ExpressionType[] {};

			// The "step"-keyword, used in the FromToLoop
			case STEP -> new ExpressionType[] {};
			});
		} else {
			setExpectedExpressions(switch (myType) {
			// Opened Array-Bracket [
			case ARRAY_START -> new ExpressionType[] { ARRAY_END, ARRAY_START, LITERAL, NAME, OPEN_BRACKET, CREMENT };

			// Closed Array-Bracket ]
			case ARRAY_END -> new ExpressionType[] { COMMA, KEYWORD, CLOSE_BRACKET, ASSIGNMENT, NAME, INFIX_OPERATOR,
					ARRAY_END, ARRAY_START, OPEN_SCOPE, CREMENT };

			// Open and Closing Vertical Separator |
			case MULTI_CALL_LINE -> new ExpressionType[] {};

			// A comma, used in Arrays, Calls, Declarations, etc..
			case COMMA -> new ExpressionType[] { EXPECTED_TYPE, LITERAL, NAME, ARRAY_START };

			// An Arrow at the end of a func-declaration, used to indicate an oncoming
			// return type.
			case EXPECTED_RETURN_TYPE -> new ExpressionType[] { EXPECTED_TYPE };

			case OPEN_BRACKET -> new ExpressionType[] { LITERAL, OPEN_BRACKET, CLOSE_BRACKET, EXPECTED_TYPE, NAME,
					ARRAY_START, CREMENT };

			case CLOSE_BRACKET -> new ExpressionType[] { CLOSE_BRACKET, EXPECTED_RETURN_TYPE, COMMA, OPEN_SCOPE,
					INFIX_OPERATOR, ARRAY_END };
			});
		}
	}

	@Override
	public final boolean isDefiniteMainExpression() {
		return false;
	}

	/**
	 * Creates a BuilderExpression from a String that isn't a Keyword.
	 * @return null if the String doesn't match any {@link ExpressionType}.
	 */
	public static BuilderExpression create(String arg) {
		ExpressionType t = isBuilderType(arg);
		if (t == KEYWORD)
			throw new AssertionError("Keywords should get handled by the enum KeywordType.");
		if (t != null)
			return new BuilderExpression(t);
		return null;
	}

	/**
	 * Returns the matching ExpressionType or null if there isnt one. Used in
	 * {@link BuilderExpression#create(String)}.
	 */
	private static ExpressionType isBuilderType(String arg) {
		for (ExpressionType t : BUILDER_EXPRESSION_TYPES) {
			if (t.toString().equals(arg))
				return t;
		}
		return null;
	}

}
