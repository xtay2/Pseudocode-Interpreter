package building.expressions.normal.operators;

import static building.types.abstractions.SpecificType.MERGED;

import java.util.List;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.Operatable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.ComparativeOperator;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.infix.LogicalOperator;
import building.types.specific.operators.InfixOpType;
import interpreting.modules.merger.SuperMerger;
import runtime.datatypes.Value;

/**
 * Consist of n Operators and n + 1 ValueHolders.
 */
public final class Operation extends Expression implements ValueHolder {

	private final List<Operatable> operation;

	/** Gets called when an Operation is constructed in the {@link SuperMerger}. */
	public Operation(int lineID, List<Operatable> e) {
		super(lineID, MERGED);
		if (e.size() < 3)
			throw new AssertionError("An operation has to atleast contain one operator and two values.\nWas " + e);
		this.operation = e;
		convertComparative();
	}

	/** Converts multiple ComparativeOperators */
	private void convertComparative() {
		for (int i = 1; i < operation.size(); i += 2) {
			if (operation.get(i) instanceof ComparativeOperator && i + 2 < operation.size()
					&& operation.get(i + 2) instanceof ComparativeOperator) {
				operation.add(i + 2, new LogicalOperator(lineIdentifier, InfixOpType.AND));
				operation.add(i + 3, operation.get(i + 1));
				i += 2;
			}
		}
	}

	@Override
	public Value getValue() {
		return recValue((ValueHolder) operation.get(0), (InfixOperator) operation.get(1), (ValueHolder) operation.get(2), 0);
	}

	/** Execute the operation in the correct order. */
	private Value recValue(ValueHolder a, InfixOperator o, ValueHolder b, int indexOfA) {
		if (indexOfA + 3 < operation.size()) { // Associativity-Check
			InfixOperator next = (InfixOperator) operation.get(indexOfA + 3);
			ValueHolder c = (ValueHolder) operation.get(indexOfA + 4);
			if (next.op.rank > o.op.rank || o.isRightAssociative())
				return o.perform(a, recValue(b, next, c, indexOfA + 2));
			return recValue(o.perform(a, b), next, c, indexOfA + 2);
		}
		return o.perform(a, b);
	}
}
