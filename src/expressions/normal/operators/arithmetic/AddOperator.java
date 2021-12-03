package expressions.normal.operators.arithmetic;

import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;

public class AddOperator extends Operator {

	public AddOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if(fst.getType() == Type.TEXT || sec.getType() == Type.TEXT)
			return new Value(fst.asText() + sec.asText(), Type.TEXT);
		if(fst.asNr() instanceof Integer && sec.asNr() instanceof Integer)
			return new Value(fst.asInt() + sec.asInt(), Type.NUMBER);
		return new Value(fst.asDouble() + sec.asDouble(), Type.NUMBER);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
