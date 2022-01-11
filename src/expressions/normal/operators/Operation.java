package expressions.normal.operators;

import java.util.ArrayList;

import datatypes.Value;
import expressions.normal.operators.comparative.ComparativeOperator;
import expressions.normal.operators.logic.AndOperator;
import expressions.special.Expression;
import expressions.special.ValueHolder;

/**
 * Consist of n Operators and n + 1 ValueHolders.
 */
public final class Operation extends Expression implements ValueHolder {

	private final ArrayList<Expression> operation;

	public Operation(int line, ArrayList<Expression> op) {
		super(line);
		if (op.size() < 3)
			throw new AssertionError("An operation has to atleast contain one operator and two values.\nWas " + op);
		this.operation = op;
		convertComparative();
	}

	/** Converts multiple ComparativeOperators */
	private void convertComparative() {
		for (int i = 1; i < operation.size(); i += 2) {
			if (operation.get(i) instanceof ComparativeOperator && i + 2 < operation.size()
					&& operation.get(i + 2) instanceof ComparativeOperator) {
				operation.add(i + 2, new AndOperator(lineIdentifier, InfixOperator.AND));
				operation.add(i + 3, operation.get(i + 1));
				i += 2;
			}
		}
	}

	@Override
	public Value getValue() {
		return recValue((ValueHolder) operation.get(0), (Operator) operation.get(1), (ValueHolder) operation.get(2), 0);
	}

	/** Execute the operation in the correct order. */
	private Value recValue(ValueHolder a, Operator o, ValueHolder b, int indexOfA) {
		if (indexOfA + 3 < operation.size()) { // Associativity-Check
			Operator next = (Operator) operation.get(indexOfA + 3);
			ValueHolder c = (ValueHolder) operation.get(indexOfA + 4);
			if (next.op.rank > o.op.rank || o.isRightAssociative()) {
				return o.perform(a, recValue(b, next, c, indexOfA + 2));
			}
			return recValue(o.perform(a, b), next, c, indexOfA + 2);
		}
		return o.perform(a, b);
	}

	@Override
	public String toString() {
		return operation.toString();
	}
}
