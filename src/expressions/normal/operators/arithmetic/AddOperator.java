package expressions.normal.operators.arithmetic;

import static types.specific.DataType.NUMBER;

import datatypes.ArrayValue;
import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import types.specific.DataType;

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
		// Array Concat
		if (fst instanceof ArrayValue a1 && sec instanceof ArrayValue a2)
			return a1.concat(a2);
		// Arithmetical Addition
		if (fst.is(NUMBER) && sec.is(NUMBER))
			return fst.asNumber().add(sec.asNumber());
		// Array Addition End
		if (fst instanceof ArrayValue a1 && !(sec instanceof ArrayValue))
			return a1.concat(sec.as((DataType) a1.type).asVarArray());
		// Array Addition Start
		if (!(fst instanceof ArrayValue) && sec instanceof ArrayValue a2)
			return fst.as((DataType) a2.type).asVarArray().concat(a2);
		return fst.asText().concat(sec.asText());
	}
}
