package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.TypeConstants;
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
		ArrayValue container = (ArrayValue) b.as(TypeConstants.VAR_ARR);
		return container.contains(element);
	}
}
