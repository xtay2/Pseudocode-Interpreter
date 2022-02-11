package expressions.normal.operators.arithmetic;

import datatypes.ArrayValue;
import datatypes.NumberValue;
import datatypes.TextValue;
import datatypes.Value;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import expressions.special.DataType;
import expressions.special.ValueHolder;

public class MultOperator extends Operator {

	public MultOperator(int line, InfixOperator mult) {
		super(line, mult);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if (fst instanceof ArrayValue arr && sec.getType() == DataType.NUMBER)
			return ArrayValue.multiply(arr, (int) sec.asInt().rawInt(), getOriginalLine());

		if (fst.getType() == DataType.NUMBER && sec instanceof ArrayValue arr)
			return ArrayValue.multiply(arr, (int) fst.asInt().rawInt(), getOriginalLine());

		if (fst.canCastTo(DataType.NUMBER) && sec.canCastTo(DataType.NUMBER))
			return NumberValue.mult(fst.asNumber(), sec.asNumber());
		
		if (fst.getType() == DataType.TEXT && sec.canCastTo(DataType.NUMBER))
			return TextValue.multiply(fst.asText(), (int) sec.asInt().rawInt(), getOriginalLine());

		if (fst.canCastTo(DataType.NUMBER) && sec.getType() == DataType.TEXT)
			return TextValue.multiply(sec.asText(), (int) fst.asInt().rawInt(), getOriginalLine());

		return NumberValue.mult(fst.asNumber(), sec.asNumber());
	}
}
