package expressions.normal.operators.postfix;

import static datatypes.numerical.NumberValue.ONE;

import datatypes.Value;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import types.specific.operators.PostfixOpType;

public class PostfixOperator extends PossibleMainExpression implements ValueHolder {

	private final ValueChanger val;

	public PostfixOperator(int lineID, PostfixOpType type, ValueChanger val) {
		super(lineID, type);
		this.val = val;
		if (val == null)
			throw new AssertionError("Val cannot be null.");
	}

	@Override
	public Value getValue() {
		Value v = val.getValue();
		switch ((PostfixOpType) type) {
			case INC:
				val.setValue(v.asNumber().add(ONE));
				break;
			case DEC:
				val.setValue(v.asNumber().sub(ONE));
				break;
			case FAC:
				v = v.asInt().fac();
				val.setValue(v);
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
