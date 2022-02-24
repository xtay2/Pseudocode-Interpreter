package expressions.normal.operators.infix;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.OPEN_BRACKET;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import types.SuperType;
import types.specific.operators.InfixOpType;

/**
 * Used in Operation
 */
public abstract class InfixOperator extends Expression {

	public enum Associativity {
		LEFT, RIGHT;
	}

	/** Corresponding symbol to this operator; */
	public final InfixOpType op;

	protected InfixOperator(int line, InfixOpType op) {
		super(line, SuperType.INFIX_OPERATOR, LITERAL, NAME, ARRAY_START, OPEN_BRACKET);
		this.op = op;
		if (op.rank < 0)
			throw new AssertionError("Rank cannot be negative.");
	}

	public static boolean isOperator(String s) {
		for (InfixOpType op : InfixOpType.values())
			if (op.symbol.equals(s))
				return true;
		return false;
	}

	public final Associativity getAssociativity() {
		return op.associativity;
	}

	public boolean isLeftAssociative() {
		return getAssociativity() == Associativity.LEFT;
	}

	public boolean isRightAssociative() {
		return getAssociativity() == Associativity.RIGHT;
	}

	public abstract Value perform(ValueHolder a, ValueHolder b);
}