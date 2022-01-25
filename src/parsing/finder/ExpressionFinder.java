package parsing.finder;

import datatypes.Value;
import expressions.main.CloseScope;
import expressions.main.OperationAssignment;
import expressions.normal.Comma;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.Expression;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.array.ArrayEnd;
import expressions.normal.array.ArrayStart;
import expressions.normal.brackets.CloseBracket;
import expressions.normal.brackets.OpenBracket;
import expressions.normal.brackets.OpenScope;
import expressions.normal.operators.Operator;
import expressions.possible.Assignment;
import expressions.possible.Crement;
import expressions.special.DataType;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;
import parsing.program.ValueBuilder;

public class ExpressionFinder {

	/**
	 * Find the matching Expression out of the input and what is expected.
	 *
	 * @param current  is the currently read string.
	 * @param expected is an array of expected expressions.
	 * @return the matching Expression
	 * @throws IllegalArgumentException if no matching expression was found.
	 */
	public static Expression find(String current, ExpressionType[] expected, int line) {
		for (ExpressionType expT : expected) {
			Expression exp = matches(current, expT, line);
			if (exp != null)
				return exp;
		}
		return null;
	}

	/**
	 * Checks, if a String matches the expected expression.
	 *
	 * @param arg  is the String.
	 * @param exp  is the exprected expression.
	 * @param lineID is the lineID
	 * @return the expression or {@code null} if the String doesnt match.
	 */
	private static Expression matches(String arg, ExpressionType exp, int lineID) {
		return switch (exp) {
		case KEYWORD:
			yield KeywordType.buildKeywordExpressionFromString(arg, lineID);
		case NAME:
			if (Name.isName(arg))
				yield new Name(arg, lineID);
			yield null;
		case LITERAL:
			if (Value.isValue(arg))
				yield ValueBuilder.stringToLiteral(arg);
			yield null;
		case EXPECTED_TYPE:
			if (DataType.isType(arg))
				yield new ExpectedType(arg, lineID);
			yield null;
		case INFIX_OPERATOR:
			if (Operator.isOperator(arg))
				yield Operator.operatorExpression(arg, lineID);
			yield null;
		case ASSIGNMENT:
			if ("=".equals(arg))
				yield new Assignment(lineID);
			yield null;
		case OPERATION_ASSIGNMENT:
			OperationAssignment.Type type = OperationAssignment.Type.getType(arg);
			if (type != null)
				yield new OperationAssignment(lineID, type);
			yield null;
		case OPEN_BRACKET:
			if ("(".equals(arg))
				yield new OpenBracket(lineID);
			yield null;
		case CLOSE_BRACKET:
			if (")".equals(arg))
				yield new CloseBracket(lineID);
			yield null;
		case OPEN_SCOPE:
			if ("{".equals(arg))
				yield new OpenScope(lineID);
			yield null;
		case CLOSE_SCOPE:
			if ("}".equals(arg))
				yield new CloseScope(lineID);
			yield null;
		case ARRAY_START:
			if ("[".equals(arg))
				yield new ArrayStart(lineID);
			yield null;
		case ARRAY_END:
			if ("]".equals(arg))
				yield new ArrayEnd(lineID);
			yield null;
		case COMMA:
			if (",".equals(arg))
				yield new Comma(lineID);
			yield null;
		case EXPECTED_RETURN_TYPE:
			if ("->".equals(arg))
				yield new ExpectedReturnType(lineID);
			yield null;
		case LOOP_CONNECTOR:
			if ("to".equals(arg) || "|".equals(arg))
				yield new LoopConnector(lineID);
			yield null;
		case CREMENT:
			if ("++".equals(arg))
				yield new Crement(Crement.Change.INC, lineID);
			if ("--".equals(arg))
				yield new Crement(Crement.Change.DEC, lineID);
			yield null;
		};
	}
}
