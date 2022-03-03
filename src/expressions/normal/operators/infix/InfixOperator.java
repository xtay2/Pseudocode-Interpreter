package expressions.normal.operators.infix;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.Operatable;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operation;
import types.specific.operators.InfixOpType;

/**
 * Used in an {@link Operation}.
 */
public abstract class InfixOperator extends Expression implements Operatable {

	public enum Associativity {
		LEFT, RIGHT;
	}

	/** Corresponding symbol to this operator; */
	public final InfixOpType op;

	protected InfixOperator(int line, InfixOpType op) {
		super(line, op);
		this.op = op;
		if (op.rank < 0)
			throw new AssertionError("Rank cannot be negative.");
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