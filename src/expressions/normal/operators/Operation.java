package expressions.normal.operators;

import java.util.ArrayList;

import expressions.special.Expression;
import expressions.special.Value;
import expressions.special.ValueHolder;

/**
 * Consist of n Operators and n + 1 ValueHolders.
 */
public final class Operation extends Expression implements ValueHolder {

	private final ArrayList<Expression> operation;

	public Operation(int line, ArrayList<Expression> op) {
		super(line);
		this.operation = op;
		if (operation.size() < 3)
			throw new IllegalArgumentException("An operation has to atleast contain one operator and two values.\nWas " + op);
	}

	@Override
	public Value getValue() {
		return recValue((ValueHolder) operation.get(0), (Operator) operation.get(1), (ValueHolder) operation.get(2), 0);
	}

	/** Execute the operation in the correct order. */
	private Value recValue(ValueHolder a, Operator o, ValueHolder b, int indexOfA) {
		if (indexOfA + 3 < operation.size()) {
			Operator next = (Operator) operation.get(indexOfA + 3);
			ValueHolder c = (ValueHolder) operation.get(indexOfA + 4);
			if (next.rank > o.rank && next.isLeftAssociative())
				return o.perform(a, recValue(b, next, c, indexOfA + 2));
			return recValue(o.perform(a, b), next, c, indexOfA + 2);
		}
		return o.perform(a, b);
	}

	@Override
	public String toString() {
		return operation.toString();
	}
}
