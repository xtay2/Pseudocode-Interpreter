package expressions.normal.operators;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.OPEN_BRACKET;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.OperationAssignment.Type;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import expressions.normal.operators.arithmetic.AddOperator;
import expressions.normal.operators.arithmetic.DivOperator;
import expressions.normal.operators.arithmetic.ModOperator;
import expressions.normal.operators.arithmetic.MultOperator;
import expressions.normal.operators.arithmetic.PowOperator;
import expressions.normal.operators.arithmetic.RootOperator;
import expressions.normal.operators.arithmetic.SubOperator;
import expressions.normal.operators.comparative.ComparativeOperator;
import expressions.normal.operators.logic.LogicalOperator;
import types.specific.ExpressionType;

/**
 * Used in Operation
 */
public abstract class Operator extends Expression {

	public enum Associativity {
		LEFT, NONE, RIGHT;
	}

	/** Corresponding symbol to this operator; */
	public final InfixOperator op;

	protected Operator(int line, InfixOperator op) {
		super(line, ExpressionType.INFIX_OPERATOR, LITERAL, NAME, ARRAY_START, OPEN_BRACKET);
		this.op = op;
		if (op.rank < 0)
			throw new AssertionError("Rank cannot be negative.");
	}

	public static boolean isOperator(String s) {
		for (InfixOperator op : InfixOperator.values())
			if (op.symbol.equals(s))
				return true;
		return false;
	}

	/**
	 * Builds a {@link Operator} from a {@link String}.
	 * 
	 * Called in {@link ExpressionType#create(String, int)}
	 * 
	 * Returns null if the {@link String} doesn't match any {@link Type}.
	 */
	public static Operator stringToOperator(String s, int line) {
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
		if (InfixOperator.ROOT.symbol.equals(s))
			return new RootOperator(line, InfixOperator.ROOT);
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
		if (InfixOperator.IN.symbol.equals(s))
			return new InOperator(line, InfixOperator.IN);
		return null;
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