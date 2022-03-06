package building.expressions.normal.operators.prefix;

import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.PrefixOpType;
import runtime.datatypes.Value;

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
