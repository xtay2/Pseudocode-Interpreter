package expressions.normal.operators.arithmetic;

import datatypes.ArrayValue;
import datatypes.Value;
import datatypes.NumberValue;
import datatypes.TextValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.ValueHolder;

public class AddOperator extends Operator {

	public AddOperator(int line, InfixOperator add) {
		super(line, add);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if (fst.canCastTo(Type.NUMBER) && sec.canCastTo(Type.NUMBER))
			return NumberValue.add(fst.asNumber(), sec.asNumber());
		
		if (fst instanceof ArrayValue a1 && sec instanceof ArrayValue a2)
			return ArrayValue.concat(a1, a2);

		if (fst instanceof ArrayValue a1 && !(sec instanceof ArrayValue))
			return ArrayValue.concat(a1, new ArrayValue(a1.getType(), sec.asVarArray()));

		if (!(fst instanceof ArrayValue) && sec instanceof ArrayValue a2)
			return ArrayValue.concat(new ArrayValue(a2.getType(), fst.asVarArray()), a2);
		return TextValue.concat(fst.asText(), sec.asText());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
