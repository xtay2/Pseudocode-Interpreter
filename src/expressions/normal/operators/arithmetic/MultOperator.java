package expressions.normal.operators.arithmetic;

import datatypes.ArrayValue;
import datatypes.Castable;
import datatypes.NumberValue;
import datatypes.TextValue;
import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.ValueHolder;

public class MultOperator extends Operator {

	public MultOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		Castable fst = a.getValue();
		Castable sec = b.getValue();
		if (fst instanceof ArrayValue arr && sec.getType() == Type.NUMBER)
			return ArrayValue.multiply(arr, (int) sec.asInt().rawInt());

		if (fst.getType() == Type.NUMBER && sec instanceof ArrayValue arr)
			return ArrayValue.multiply(arr, (int) sec.asInt().rawInt());

		if (fst.getType() == Type.TEXT && sec.getType() == Type.NUMBER)
			return TextValue.multiply(fst.asText(), (int) sec.asInt().rawInt());

		if (fst.getType() == Type.NUMBER && sec.getType() == Type.TEXT)
			return TextValue.multiply(sec.asText(), (int) fst.asInt().rawInt());

		return NumberValue.mult(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
