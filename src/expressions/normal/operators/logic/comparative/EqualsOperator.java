package expressions.normal.operators.logic.comparative;

import datatypes.Castable;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class EqualsOperator extends Operator implements ComparativeOperator {

	public EqualsOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return a.getValue().eq(b.getValue());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
