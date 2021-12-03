package expressions.normal.operators.logic;

import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;

public class LessOperator extends Operator {

	public LessOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return new Value(a.getValue().asInt() < b.getValue().asInt(), Type.BOOL);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
