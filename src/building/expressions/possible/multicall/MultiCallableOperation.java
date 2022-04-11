package building.expressions.possible.multicall;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import runtime.datatypes.Value;

/**
 * @see InfixOperator
 */
public interface MultiCallableOperation extends MultiCallable {

	/**
	 * Similar to {@link #executeFor(ValueHolder[])}, but takes an additional operand on the right.
	 * 
	 * <pre>
	 * |1, 2, 3| - a
	 * </pre>
	 */
	Value executeFor(ValueHolder[] content, ValueHolder operand);

	/**
	 * Similar to {@link #executeFor(ValueHolder[])}, but takes an additional operand on the left.
	 * 
	 * <pre>
	 * a - |1, 2, 3|
	 * </pre>
	 */
	Value executeFor(ValueHolder operand, ValueHolder[] content);
}
