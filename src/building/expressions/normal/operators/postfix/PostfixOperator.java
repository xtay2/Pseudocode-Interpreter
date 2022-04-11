package building.expressions.normal.operators.postfix;

import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.prefix.PrefixOperator;
import building.expressions.possible.multicall.MultiCall;
import building.expressions.possible.multicall.MultiCallableValueHolder;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.operators.PostfixOpType;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/**
 * @see PrefixOperator
 * @see InfixOperator
 */
public class PostfixOperator extends PossibleMainExpression implements MultiCallableValueHolder {

	private final ValueHolder content;

	public PostfixOperator(int lineID, PostfixOpType type, ValueHolder content) {
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
		return new ArrayValue(ArrayType.VAR_ARRAY, res);
	}

	private Value evaluate(ValueHolder val) {
		Value v = val.getValue();
		switch ((PostfixOpType) type) {
			case INC:
				if (val instanceof ValueChanger incVar)
					incVar.setValue(v.asNumber().add(ONE));
				else
					throw new IllegalCodeFormatException(getOriginalLine(), "You cannot post-increment a " + ((Expression) val).type + ".");
				break;
			case DEC:
				if (val instanceof ValueChanger decVar)
					decVar.setValue(v.asNumber().sub(ONE));
				else
					throw new IllegalCodeFormatException(getOriginalLine(), "You cannot post-decrement a " + ((Expression) val).type + ".");
				break;
			case FAC:
				v = v.asInt().fac();
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
