package expressions.normal.operators.arithmetic;

import static types.specific.DataType.NUMBER;
import static types.specific.DataType.TEXT;

import datatypes.ArrayValue;
import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

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

		// Array-Multiplication
		if (fst instanceof ArrayValue arr && sec.is(NUMBER))
			return arr.multiply(sec.asInt().value.intValueExact(), getOriginalLine());

		// Array-Multiplication
		if (fst.is(NUMBER) && sec instanceof ArrayValue arr)
			return arr.multiply(fst.asInt().value.intValueExact(), getOriginalLine());

		// Arithmetical Addition
		if (fst.canCastTo(NUMBER) && sec.canCastTo(NUMBER))
			return fst.asNumber().mult(sec.asNumber());

		// Text-Multiplication
		if (fst.is(TEXT) && sec.canCastTo(NUMBER))
			return fst.asText().multiply(sec.asInt().value.intValueExact(), getOriginalLine());

		// Text-Multiplication
		if (fst.canCastTo(NUMBER) && sec.is(TEXT))
			return sec.asText().multiply(fst.asInt().value.intValueExact(), getOriginalLine());

		return fst.asNumber().mult(sec.asNumber());
	}
}
