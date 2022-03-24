package building.expressions.normal.operators.postfix;

import static building.types.specific.operators.PostfixOpType.INC;
import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.prefix.PrefixOperator;
import building.types.specific.operators.PostfixOpType;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.Value;

/**
 * @see PrefixOperator
 * @see InfixOperator
 */
public class PostfixOperator extends PossibleMainExpression implements ValueHolder {

	private final ValueHolder val;

	public PostfixOperator(int lineID, PostfixOpType type, ValueHolder val) {
		super(lineID, type);
		this.val = val;
		if (val == null)
			throw new AssertionError("Val cannot be null.");
	}

	@Override
	public Value getValue() {
		Value v = val.getValue();
		switch ((PostfixOpType) type) {
		case INC, DEC:
			crement(v, type == INC);
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

	/**
	 * Post-in- or decrements the value.
	 * 
	 * @param v     is the value
	 * @param isInc is true if the value should get incremented and false if it
	 *              should get decremented.
	 * @throws IllegalCodeFormatException if the target isn't writable, for example:
	 *                                    5++
	 */
	private void crement(Value v, boolean isInc) {
		if (val instanceof ValueChanger vc)
			vc.setValue(isInc ? v.asNumber().add(ONE) : v.asNumber().sub(ONE));
		else
			throw new IllegalCodeFormatException(getOriginalLine(),
					"You cannot post-" + (isInc ? "in" : "de") + "crement a " + ((Expression) val).type + ".");
	}
}
