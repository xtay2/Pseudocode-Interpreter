package expressions.normal.operators;

import static types.specific.DataType.isArrayType;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import types.specific.DataType;

public class InOperator extends Operator {

	protected InOperator(int line, InfixOperator op) {
		super(line, op);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
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
