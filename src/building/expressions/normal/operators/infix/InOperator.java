package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import misc.constants.TypeConstants;
import runtime.datatypes.BoolValue;
import runtime.datatypes.array.ArrayValue;

public class InOperator extends InfixOperator {

	public InOperator(int lineID, InfixOpType op) {
		super(lineID, op);
	}

	@Override
	public BoolValue perform(ValueHolder a, ValueHolder b) {
		return ((ArrayValue) b.as(TypeConstants.VAR_ARR)).contains(a.getValue());
	}
}
