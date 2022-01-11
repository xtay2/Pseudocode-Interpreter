package expressions.normal.operators;

import datatypes.Value;
import expressions.normal.operators.arithmetic.*;
import expressions.normal.operators.comparative.*;
import expressions.normal.operators.logic.*;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import helper.Output;
import parsing.program.ExpressionType;

/**
 * Used in Operation
 */
public abstract class Operator extends Expression {

	public enum Associativity {
		NONE, LEFT, RIGHT;
	}

	
	/** Corresponding symbol to this operator; */
	public final InfixOperator op;

	protected Operator(int line, InfixOperator op) {
		super(line);
		this.op = op;
		if (op.rank < 0)
			throw new AssertionError("Rank cannot be negative.");
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START, ExpressionType.OPEN_BRACKET);
	}

	public abstract Value perform(ValueHolder a, ValueHolder b);

	public abstract Associativity getAssociativity();

	public boolean isLeftAssociative() {
		return getAssociativity() == Associativity.LEFT;
	}

	public boolean isRightAssociative() {
		return getAssociativity() == Associativity.RIGHT;
	}

	
	public static boolean isOperator(String s) {
		for (InfixOperator op : InfixOperator.values())
			if (op.symbol.equals(s))
				return true;
		return false;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : op.toString();
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
			return new EqualsOperator(line, InfixOperator.EQUALS);
		if (InfixOperator.NOT_EQUALS.symbol.equals(s))
			return new NotEqualsOperator(line, InfixOperator.NOT_EQUALS);
		if (InfixOperator.LESS.symbol.equals(s))
			return new LessOperator(line, InfixOperator.LESS);
		if (InfixOperator.LESS_EQ.symbol.equals(s))
			return new LessEqOperator(line, InfixOperator.LESS_EQ);
		if (InfixOperator.GREATER.symbol.equals(s))
			return new GreaterOperator(line, InfixOperator.GREATER);
		if (InfixOperator.GREATER_EQ.symbol.equals(s))
			return new GreaterEqOperator(line, InfixOperator.GREATER_EQ);
		/* Logic */
		if (InfixOperator.AND.symbol.equals(s))
			return new AndOperator(line, InfixOperator.AND);
		if (InfixOperator.NAND.symbol.equals(s))
			return new NandOperator(line, InfixOperator.NAND);
		if (InfixOperator.OR.symbol.equals(s))
			return new OrOperator(line, InfixOperator.OR);
		if (InfixOperator.NOR.symbol.equals(s))
			return new NorOperator(line, InfixOperator.NOR);
		if (InfixOperator.XOR.symbol.equals(s))
			return new XorOperator(line, InfixOperator.XOR);
		throw new AssertionError(s + " should be known by now.");
	}
}