package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

public class InOperator extends InfixOperator {

	public InOperator(int lineID, InfixOpType op) {
		super(lineID, op);
	}

	@Override
	public BoolValue perform(ValueHolder a, ValueHolder b) {
		Value element = a.getValue();
		Value container = b.getValue();
		if (container.type instanceof ArrayType)
			return container.asVarArray().contains(element);
		return container.asText().contains(element);
	}
}
