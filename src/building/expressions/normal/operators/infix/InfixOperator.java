package building.expressions.normal.operators.infix;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.Operatable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.Operation;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.Value;

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