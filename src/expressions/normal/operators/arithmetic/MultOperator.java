package expressions.normal.operators.arithmetic;

import datatypes.ArrayValue;
import datatypes.Value;
import datatypes.NumberValue;
import datatypes.TextValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.Type;
import expressions.special.ValueHolder;

public class MultOperator extends Operator {

	public MultOperator(int line, InfixOperator mult) {
		super(line, mult);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		if (fst instanceof ArrayValue arr && sec.getType() == Type.NUMBER)
			return ArrayValue.multiply(arr, (int) sec.asInt().rawInt(), getOriginalLine());

		if (fst.getType() == Type.NUMBER && sec instanceof ArrayValue arr)
			return ArrayValue.multiply(arr, (int) fst.asInt().rawInt(), getOriginalLine());

		if (fst.canCastTo(Type.NUMBER) && sec.canCastTo(Type.NUMBER))
			return NumberValue.mult(fst.asNumber(), sec.asNumber());
		
		if (fst.getType() == Type.TEXT && sec.canCastTo(Type.NUMBER))
			return TextValue.multiply(fst.asText(), (int) sec.asInt().rawInt(), getOriginalLine());

		if (fst.canCastTo(Type.NUMBER) && sec.getType() == Type.TEXT)
			return TextValue.multiply(sec.asText(), (int) fst.asInt().rawInt(), getOriginalLine());

		return NumberValue.mult(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
