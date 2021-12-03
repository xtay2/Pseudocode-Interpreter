package expressions.normal.operators.logic;

import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;

public class EqualsOperator extends Operator {

	public EqualsOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if (fst.asText().equals(sec.asText()))
			return new Value(true, Type.BOOL);
		return new Value(false, Type.BOOL);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
