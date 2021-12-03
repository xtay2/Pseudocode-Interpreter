package expressions.normal.operators;

import expressions.normal.operators.arithmetic.AddOperator;
import expressions.normal.operators.arithmetic.DivOperator;
import expressions.normal.operators.arithmetic.MultOperator;
import expressions.normal.operators.arithmetic.SubOperator;
import expressions.normal.operators.logic.AndOperator;
import expressions.normal.operators.logic.EqualsOperator;
import expressions.normal.operators.logic.GreaterEqOperator;
import expressions.normal.operators.logic.GreaterOperator;
import expressions.normal.operators.logic.LessEqOperator;
import expressions.normal.operators.logic.LessOperator;
import expressions.normal.operators.logic.NotEqualsOperator;
import expressions.normal.operators.logic.OrOperator;
import expressions.special.Expression;
import expressions.special.Value;
import expressions.special.ValueHolder;
import parser.program.ExpressionType;

/**
 * Used in Operation
 */
public abstract class Operator extends Expression {

	public enum Associativity {
		LEFT, RIGHT, NONE;
	}

	/** Value that determines the importance of an Operator. */
	public final int rank;

	protected Operator(int line, int rank) {
		super(line);
		if (rank < 0)
			throw new IllegalArgumentException("Rank cannot be negative.");
		this.rank = rank;
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	public abstract Value perform(ValueHolder a, ValueHolder b);

	public abstract Associativity getAssociativity();

	public boolean isLeftAssociative() {
		return getAssociativity() == Associativity.LEFT;
	}

	enum InfixOperator {
		/** Arithmetic */
		ADD("+", 6), SUB("-", 6), MULT("*", 7), DIV("/", 7), MOD("%", 7),

		/** Zuweisungen */
		ADDI("+=", 1), SUBI("-=", 1), MULTI("*=", 1), DIVI("/=", 1), MODI("%=", 1),

		/** Comparison */
		EQUALS("==", 4), NOT_EQUALS("!=", 4), LESS("<", 5), LESS_EQ("<=", 5), GREATER(">", 5), GREATER_EQ(">=", 5),

		/** Logic */
		AND("and", 3), NAND("nand", 3), OR("or", 2), NOR("nor", 2), XOR("xor", 2);

		public final String symbol;

		public final int rank;

		InfixOperator(String s, int rank) {
			symbol = s;
			this.rank = rank;
		}
	}

	public static boolean isOperator(String s) {
		for (InfixOperator op : InfixOperator.values())
			if (op.symbol.equals(s))
				return true;
		return false;
	}

	public static Operator operatorExpression(String s, int line) {
		/* Arithmetic */
		if (InfixOperator.ADD.symbol.equals(s))
			return new AddOperator(line, InfixOperator.ADD.rank);
		if (InfixOperator.MULT.symbol.equals(s))
			return new MultOperator(line, InfixOperator.MULT.rank);
		if (InfixOperator.SUB.symbol.equals(s))
			return new SubOperator(line, InfixOperator.SUB.rank);
		if (InfixOperator.DIV.symbol.equals(s))
			return new DivOperator(line, InfixOperator.DIV.rank);
		/** Comparison */
		if (InfixOperator.EQUALS.symbol.equals(s))
			return new EqualsOperator(line, InfixOperator.EQUALS.rank);
		if (InfixOperator.NOT_EQUALS.symbol.equals(s))
			return new NotEqualsOperator(line, InfixOperator.NOT_EQUALS.rank);
		if (InfixOperator.LESS.symbol.equals(s))
			return new LessOperator(line, InfixOperator.LESS.rank);
		if (InfixOperator.LESS_EQ.symbol.equals(s))
			return new LessEqOperator(line, InfixOperator.LESS_EQ.rank);
		if (InfixOperator.GREATER.symbol.equals(s))
			return new GreaterOperator(line, InfixOperator.GREATER.rank);
		if (InfixOperator.GREATER_EQ.symbol.equals(s))
			return new GreaterEqOperator(line, InfixOperator.GREATER_EQ.rank);
		/* Logic */
		if (InfixOperator.AND.symbol.equals(s))
			return new AndOperator(line, InfixOperator.AND.rank);
		if (InfixOperator.OR.symbol.equals(s))
			return new OrOperator(line, InfixOperator.OR.rank);
		throw new AssertionError(s + " should be known by now.");
	}

}
