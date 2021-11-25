package programreader.finder;

import programreader.expressions.main.CloseBlock;
import programreader.expressions.main.Declaration;
import programreader.expressions.normal.*;
import programreader.expressions.special.Expression;
import programreader.expressions.special.Operator;
import programreader.expressions.special.Type;
import programreader.expressions.special.Value;
import programreader.program.ExpressionType;

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
		case NAME:
			if (Name.isName(arg))
				yield new Name(arg, line);
			yield null;
		case LITERAL:
			if (Value.isValue(arg))
				yield new Literal(arg, line);
			yield null;
		case TYPED_VAR:
			if (Type.isType(arg))
				yield new TypedVar(arg, line);
			yield null;
		case EXPECTED_TYPE:
			if (Type.isType(arg))
				yield new ExpectedType(arg, line);
			yield null;
		case INFIX_OPERATOR:
			if(Operator.isOperator(arg))
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
		case COMMA:
			if (",".equals(arg))
				yield new Comma(line);
			yield null;
		case EXPECTED_RETURN_TYPE:
			if ("->".equals(arg))
				yield new ExpectedReturnType(line);
			yield null;
		case ONE_LINE_STATEMENT:
			if (":".equals(arg))
				yield new OneLineStatement(line);
			yield null;
		default:
			yield null;
		};
	}
}
