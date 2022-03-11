package building.expressions.possible.multicall;

import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.InfixOperator;
import runtime.datatypes.Value;

/**
 * Target for any {@link MultiCall}.
 */
public interface MultiCallable {

	/**
	 * Should call {@link MultiCallable#execute(Value)} once for every element in content.
	 * 
	 * <pre>
	 * -Executes this {@link MultiCallable} once. 
	 * -Is similar to {@link MainExpression#execute()} but cannot be used interchangeably!
	 * -Gets called by {@link MultiCall#execute()}.
	 * </pre>
	 * 
	 * @param content is the array of parameters that get passed, one at a time.
	 * @return Value is an optional return-value that gets passed by a {@link ValueHolder} or an
	 *         {@link InfixOperator}.
	 */
	Value executeFor(ValueHolder[] content);
}