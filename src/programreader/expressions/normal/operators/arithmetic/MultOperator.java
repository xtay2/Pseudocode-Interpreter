package programreader.expressions.normal.operators.arithmetic;

import programreader.expressions.special.Operator;
import programreader.expressions.special.Type;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;

public class MultOperator extends Operator {

	public MultOperator(int line) {
		super(line, 0);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if (fst.getType() == Type.TEXT && sec.getType() == Type.NUMBER)
			return new Value(fst.asText().repeat(sec.asInt()), Type.TEXT);
		if (fst.getType() == Type.NUMBER && sec.getType() == Type.TEXT)
			return new Value(sec.asText().repeat(fst.asInt()), Type.TEXT);
		if (fst.asNr() instanceof Integer && sec.asNr() instanceof Integer)
			return new Value(fst.asInt() * sec.asInt(), Type.NUMBER);
		return new Value(fst.asDouble() * sec.asDouble(), Type.NUMBER);
	}
	
	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
