package expressions.normal.operators.infix;

import static types.specific.DataType.isArrayType;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import types.specific.DataType;
import types.specific.operators.InfixOpType;

public class InOperator extends InfixOperator {

	public InOperator(int line, InfixOpType op) {
		super(line, op);
	}
	
	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value element = a.getValue();
		Value container = b.getValue();
		if (isArrayType((DataType) container.type))
			return container.asVarArray().contains(element);
		return container.asText().contains(element);
	}

}
