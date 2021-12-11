package expressions.normal.operators.arithmetic;

import java.util.List;

import datatypes.ArrayValue;
import datatypes.Castable;
import datatypes.NumberValue;
import datatypes.TextValue;
import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.ValueHolder;

public class AddOperator extends Operator {

	public AddOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		Castable fst = a.getValue();
		Castable sec = b.getValue();
		if (fst.getType() == Type.TEXT || sec.getType() == Type.TEXT)
			return TextValue.concat(fst.asText(), sec.asText());

		if (fst instanceof ArrayValue a1 && sec instanceof ArrayValue a2)
			return ArrayValue.concat(a1, a2);

		if (fst instanceof ArrayValue a1 && !(sec instanceof ArrayValue))
			return ArrayValue.concat(a1, new ArrayValue(fst.getType(), List.of(sec), true));

		if (!(fst instanceof ArrayValue) && sec instanceof ArrayValue a2)
			return ArrayValue.concat(new ArrayValue(sec.getType(), List.of(fst), true), a2);
		
		return NumberValue.add(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
