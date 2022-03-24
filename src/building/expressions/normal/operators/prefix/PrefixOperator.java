package building.expressions.normal.operators.prefix;

import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.postfix.PostfixOperator;
import building.types.specific.operators.PrefixOpType;
import runtime.datatypes.Value;

/**
 * @see PostfixOperator
 * @see InfixOperator
 */
public class PrefixOperator extends PossibleMainExpression implements ValueHolder {

	private final ValueHolder val;

	public PrefixOperator(int lineID, PrefixOpType type, ValueHolder val) {
		super(lineID, type);
		this.val = val;
		if (val == null)
			throw new AssertionError("Val cannot be null.");
	}

	@Override
	public Value getValue() {
		Value v = val.getValue();
		switch ((PrefixOpType) type) {
		case INC:
			v = v.asNumber().add(ONE);
			if (val instanceof ValueChanger vc)
				vc.setValue(v);
			break;
		case DEC:
			v = v.asNumber().sub(ONE);
			if (val instanceof ValueChanger vc)
				vc.setValue(v);
			break;
		case NEG:
			v = v.asNumber().negate();
			break;
		case NOT:
			v = v.asBool().not();
			break;
		}
		return v;
	}

	@Override
	public boolean execute() {
		getValue();
		return callNextLine();
	}

}
