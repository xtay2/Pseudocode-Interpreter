package building.expressions.normal.operators.prefix;

import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.postfix.PostfixOperator;
import building.expressions.possible.multicall.MultiCall;
import building.expressions.possible.multicall.MultiCallableValueHolder;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.operators.PrefixOpType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/** @see PostfixOperator
 * @see InfixOperator */
public class PrefixOperator extends PossibleMainExpression implements MultiCallableValueHolder {

	private final ValueHolder content;

	public PrefixOperator(int lineID, PrefixOpType type, ValueHolder content) {
		super(lineID, type);
		this.content = content;
		if (content == null)
			throw new AssertionError("Val cannot be null.");
	}

	@Override
	public Value getValue() {
		if (content instanceof MultiCall mc)
			return executeFor(mc.content);
		return evaluate(content);
	}

	@Override
	public Value executeFor(ValueHolder[] content) {
		ValueHolder[] res = new ValueHolder[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = evaluate(content[i]);
		return new ArrayValue(ArrayType.VAR_ARRAY, false, res);
	}

	private Value evaluate(ValueHolder val) {
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
