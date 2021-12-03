package expressions.normal.operators.arithmetic;

import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;

public class DivOperator extends Operator {

	public DivOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		return new Value(fst.asDouble() / sec.asDouble(), Type.NUMBER);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

}
