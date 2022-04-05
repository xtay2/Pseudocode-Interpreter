package building.expressions.possible.multicall;

import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.postfix.PostfixOperator;
import building.expressions.normal.operators.prefix.PrefixOperator;
import building.expressions.possible.Call;
import runtime.datatypes.Value;

/**
 * Target for any {@link MultiCall}.
 * 
 * A {@link MultiCallable} should allways returns a value, and is ideally a {@link ValueHolder}.
 * 
 * 
 * @see Call
 * @see PostfixOperator
 * @see PrefixOperator
 * @see InfixOperator
 */
public interface MultiCallable extends ValueHolder {

	/**
	 * Should call {@link MultiCallable#getValue()} once for every element in content.
	 * 
	 * <pre>
	 * -Executes this {@link MultiCallable} once. 
	 * -Is similar to {@link MainExpression#getValue()} but cannot be used interchangeably!
	 * -Gets called by {@link MultiCall#getValue()}.
	 * </pre>
	 * 
	 * @param content is the array of parameters that get passed, one at a time.
	 * @return Value is an optional return-value that gets passed by a {@link ValueHolder} or an
	 *         {@link InfixOperator}.
	 */
	default Value executeFor(ValueHolder[] content) {
		throw new AssertionError("Define and call one of the three executeFor-Methods in " + getClass().getSimpleName() + ".");
	}

	/**
	 * Similar to {@link #executeFor(ValueHolder[])}, but takes an additional operand on the right.
	 * 
	 * <pre>
	 * |1, 2, 3| - a
	 * </pre>
	 */
	default Value executeFor(ValueHolder[] content, ValueHolder operand) {
		return executeFor(content);
	}

	/**
	 * Similar to {@link #executeFor(ValueHolder[])}, but takes an additional operand on the left.
	 * 
	 * <pre>
	 * a - |1, 2, 3|
	 * </pre>
	 */
	default Value executeFor(ValueHolder operand, ValueHolder[] content) {
		return executeFor(content);
	}

}
