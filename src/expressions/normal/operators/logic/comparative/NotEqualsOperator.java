package expressions.normal.operators.logic.comparative;

import datatypes.Castable;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class NotEqualsOperator extends Operator implements ComparativeOperator {

	public NotEqualsOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return a.getValue().neq(b.getValue());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
