package expressions.normal.operators.arithmetic;

import datatypes.ArrayValue;
import datatypes.NumberValue;
import datatypes.TextValue;
import datatypes.Value;
import expressions.abstractions.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import expressions.special.DataType;

public class AddOperator extends Operator {

	public AddOperator(int line, InfixOperator add) {
		super(line, add);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();

		if (fst instanceof ArrayValue a1 && sec instanceof ArrayValue a2)
			return ArrayValue.concat(a1, a2);

		if (fst instanceof ArrayValue a1 && !(sec instanceof ArrayValue))
			return ArrayValue.concat(a1, sec.as(a1.getType()).asVarArray());

		if (fst.getType() == DataType.NUMBER && sec.getType() == DataType.NUMBER)
			return NumberValue.add(fst.asNumber(), sec.asNumber());

		if (!(fst instanceof ArrayValue) && sec instanceof ArrayValue a2)
			return ArrayValue.concat(fst.as(a2.getType()).asVarArray(), a2);
		return TextValue.concat(fst.asText(), sec.asText());
	}
}
