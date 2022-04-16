package building.expressions.normal.operators.infix;

import static building.types.specific.datatypes.ArrayType.VAR_ARRAY;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

public class InOperator extends InfixOperator {

	public InOperator(int lineID, InfixOpType op) {
		super(lineID, op);
	}

	@Override
	public BoolValue perform(ValueHolder a, ValueHolder b) {
		Value element = a.getValue();
		Value container = b.getValue();
		if (container.type instanceof ArrayType)
			return ((ArrayValue) container.as(VAR_ARRAY)).contains(element);
		return container.asText().contains(element);
	}
}
