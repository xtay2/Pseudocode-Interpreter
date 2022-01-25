package expressions.normal.operators;

import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_BRACKET;

import datatypes.Value;
import expressions.normal.Expression;
import expressions.normal.operators.arithmetic.AddOperator;
import expressions.normal.operators.arithmetic.DivOperator;
import expressions.normal.operators.arithmetic.ModOperator;
import expressions.normal.operators.arithmetic.MultOperator;
import expressions.normal.operators.arithmetic.PowOperator;
import expressions.normal.operators.arithmetic.SubOperator;
import expressions.normal.operators.comparative.ComparativeOperator;
import expressions.normal.operators.logic.LogicalOperator;
import expressions.special.ValueHolder;

/**
 * Used in Operation
 */
public abstract class Operator extends Expression {

	public enum Associativity {
		LEFT, NONE, RIGHT;
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
			return new AddOperator(line, InfixOperator.ADD);
		if (InfixOperator.MULT.symbol.equals(s))
			return new MultOperator(line, InfixOperator.MULT);
		if (InfixOperator.SUB.symbol.equals(s))
			return new SubOperator(line, InfixOperator.SUB);
		if (InfixOperator.DIV.symbol.equals(s))
			return new DivOperator(line, InfixOperator.DIV);
		if (InfixOperator.MOD.symbol.equals(s))
			return new ModOperator(line, InfixOperator.MOD);
		if (InfixOperator.POW.symbol.equals(s))
			return new PowOperator(line, InfixOperator.POW);
		/** Comparison */
		if (InfixOperator.EQUALS.symbol.equals(s))
			return new ComparativeOperator(line, InfixOperator.EQUALS);
		if (InfixOperator.NOT_EQUALS.symbol.equals(s))
			return new ComparativeOperator(line, InfixOperator.NOT_EQUALS);
		if (InfixOperator.LESS.symbol.equals(s))
			return new ComparativeOperator(line, InfixOperator.LESS);
		if (InfixOperator.LESS_EQ.symbol.equals(s))
			return new ComparativeOperator(line, InfixOperator.LESS_EQ);
		if (InfixOperator.GREATER.symbol.equals(s))
			return new ComparativeOperator(line, InfixOperator.GREATER);
		if (InfixOperator.GREATER_EQ.symbol.equals(s))
			return new ComparativeOperator(line, InfixOperator.GREATER_EQ);
		/* Logic */
		if (InfixOperator.AND.symbol.equals(s))
			return new LogicalOperator(line, InfixOperator.AND);
		if (InfixOperator.NAND.symbol.equals(s))
			return new LogicalOperator(line, InfixOperator.NAND);
		if (InfixOperator.OR.symbol.equals(s))
			return new LogicalOperator(line, InfixOperator.OR);
		if (InfixOperator.NOR.symbol.equals(s))
			return new LogicalOperator(line, InfixOperator.NOR);
		if (InfixOperator.XOR.symbol.equals(s))
			return new LogicalOperator(line, InfixOperator.XOR);
		if (InfixOperator.XNOR.symbol.equals(s))
			return new LogicalOperator(line, InfixOperator.XNOR);
		throw new AssertionError(s + " should be known by now.");
	}

	/** Corresponding symbol to this operator; */
	public final InfixOperator op;

	protected Operator(int line, InfixOperator op) {
		super(line);
		this.op = op;
		if (op.rank < 0)
			throw new AssertionError("Rank cannot be negative.");
		setExpectedExpressions(LITERAL, NAME, ARRAY_START, OPEN_BRACKET);
	}

	public abstract Associativity getAssociativity();

	public boolean isLeftAssociative() {
		return getAssociativity() == Associativity.LEFT;
	}

	public boolean isRightAssociative() {
		return getAssociativity() == Associativity.RIGHT;
	}

	public abstract Value perform(ValueHolder a, ValueHolder b);
}