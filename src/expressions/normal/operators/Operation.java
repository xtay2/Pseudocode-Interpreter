package expressions.normal.operators;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.MergedExpression;
import expressions.abstractions.ValueHolder;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import expressions.normal.operators.comparative.ComparativeOperator;
import expressions.normal.operators.logic.LogicalOperator;
import parsing.program.ValueMerger;

/**
 * Consist of n Operators and n + 1 ValueHolders.
 */
public final class Operation extends Expression implements ValueHolder, MergedExpression {

	private List<Expression> operation;

	/** Gets called when an Operation is constructed in the {@link ValueMerger}. */
	public Operation(int line) {
		super(line);
	}

	@Override
	public void merge(List<Expression> e) {
		if (e.size() < 3)
			throw new AssertionError("An operation has to atleast contain one operator and two values.\nWas " + e);
		this.operation = e;
		convertComparative();
	}

	@Override
	public void merge(Expression... e) {
		if (e.length < 3)
			throw new AssertionError("An operation has to atleast contain one operator and two values.\nWas " + e);
		this.operation = new ArrayList<>(Arrays.asList(e));
		convertComparative();
	}

	/** Converts multiple ComparativeOperators */
	private void convertComparative() {
		for (int i = 1; i < operation.size(); i += 2) {
			if (operation.get(i) instanceof ComparativeOperator && i + 2 < operation.size()
					&& operation.get(i + 2) instanceof ComparativeOperator) {
				operation.add(i + 2, new LogicalOperator(lineIdentifier, InfixOperator.AND));
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
			if (next.op.rank > o.op.rank || o.isRightAssociative())
				return o.perform(a, recValue(b, next, c, indexOfA + 2));
			return recValue(o.perform(a, b), next, c, indexOfA + 2);
		}
		return o.perform(a, b);
	}
}
