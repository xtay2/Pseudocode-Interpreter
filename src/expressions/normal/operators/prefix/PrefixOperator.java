package expressions.normal.operators.prefix;

import static datatypes.numerical.NumberValue.ONE;

import datatypes.Value;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import types.specific.operators.PrefixOpType;

public class PrefixOperator extends PossibleMainExpression implements ValueHolder {

	private final ValueChanger val;

	public PrefixOperator(int lineID, PrefixOpType type, ValueChanger val) {
		super(lineID, type);
		this.val = val;
		if (val == null)
			throw new AssertionError("Val cannot be null.");
	}

	@Override
	public Value getValue() {
		Value v = val.getValue();
		v = switch ((PrefixOpType) type) {
			case INC -> v.asNumber().add(ONE);
			case DEC -> v.asNumber().sub(ONE);
			case NOT -> v.asBool().not();
		};
		val.setValue(v);
		return v;
	}

	@Override
	public boolean execute() {
		getValue();
		return callNextLine();
	}

}
