package parser.finder;

import datatypes.Value;
import expressions.main.CloseBlock;
import expressions.main.Declaration;
import expressions.normal.Comma;
import expressions.normal.Semikolon;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.Literal;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.normal.array.ArrayEnd;
import expressions.normal.array.ArrayStart;
import expressions.normal.brackets.CloseBracket;
import expressions.normal.brackets.OpenBlock;
import expressions.normal.brackets.OpenBracket;
import expressions.normal.operators.Operator;
import expressions.special.Expression;
import expressions.special.Type;
import parser.program.ExpressionType;
import parser.program.ValueBuilder;

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
	 * @param arg is the String.
	 * @param exp is the exprected expression.
	 * @return the expression or {@code null} if the String doesnt match.
	 */
	private static Expression matches(String arg, ExpressionType exp, int line) {
		return switch (exp) {
		case KEYWORD:
			if (KeywordFinder.isKeyword(arg))
				yield KeywordFinder.keywordExpression(arg, line);
			yield null;
		case VAR_TYPE:
			if (Type.isType(arg))
				yield new Variable(line, Type.stringToType(arg));
			yield null;
		case NAME:
			if (Name.isName(arg))
				yield new Name(arg, line);
			yield null;
		case LITERAL:
			if (Value.isValue(arg))
				yield new Literal(ValueBuilder.buildLiteral(arg), line);
			yield null;
		case EXPECTED_TYPE:
			if (Type.isType(arg))
				yield new ExpectedType(arg, line);
			yield null;
		case INFIX_OPERATOR:
			if (Operator.isOperator(arg))
				yield Operator.operatorExpression(arg, line);
			yield null;
		case DECLARATION:
			if ("=".equals(arg))
				yield new Declaration(line);
			yield null;
		case OPEN_BRACKET:
			if ("(".equals(arg))
				yield new OpenBracket(line);
			yield null;
		case CLOSE_BRACKET:
			if (")".equals(arg))
				yield new CloseBracket(line);
			yield null;
		case OPEN_BLOCK:
			if ("{".equals(arg))
				yield new OpenBlock(line);
			yield null;
		case CLOSE_BLOCK:
			if ("}".equals(arg))
				yield new CloseBlock(line);
			yield null;
		case ARRAY_START:
			if ("[".equals(arg))
				yield new ArrayStart(line);
			yield null;
		case ARRAY_END:
			if ("]".equals(arg))
				yield new ArrayEnd(line);
			yield null;
		case COMMA:
			if (",".equals(arg))
				yield new Comma(line);
			yield null;
		case EXPECTED_RETURN_TYPE:
			if ("->".equals(arg))
				yield new ExpectedReturnType(line);
			yield null;
		case LOOP_CONNECTOR:
			if ("to".equals(arg) || "in".equals(arg) || "|".equals(arg))
				yield new LoopConnector(line);
			yield null;
		case DEFINITE_LINEBREAK:
			if (";".equals(arg))
				yield new Semikolon(line);
			yield null;
		default:
			throw new IllegalArgumentException("Unexpected value: " + exp);
		};
	}
}