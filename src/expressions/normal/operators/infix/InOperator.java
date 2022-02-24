package expressions.normal.operators.infix;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import types.SuperType;
import types.specific.operators.InfixOpType;

public class InOperator extends InfixOperator {

	public InOperator(int line, InfixOpType op) {
		super(line, op);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value element = a.getValue();
		Value container = b.getValue();
		if (container.type.is(SuperType.ARRAY_TYPE))
			return container.asVarArray().contains(element);
		return container.asText().contains(element);
	}

}
