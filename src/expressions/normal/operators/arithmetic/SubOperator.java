package expressions.normal.operators.arithmetic;

import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;

public class SubOperator extends Operator{

	public SubOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if(fst.asNr() instanceof Integer && sec.asNr() instanceof Integer)
			return new Value(fst.asInt() - sec.asInt(), Type.NUMBER);
		return new Value(fst.asDouble() - sec.asDouble(), Type.NUMBER);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

}
