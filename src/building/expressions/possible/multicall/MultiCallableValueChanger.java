package building.expressions.possible.multicall;

import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

public interface MultiCallableValueChanger extends MultiCallableValueHolder, ValueChanger {

	/**
	 * Should call {@link MultiCallable#setValue()} once for every element in content.
	 * 
	 * <pre>
	 * -Is similar to {@link MainExpression#setValue()} but cannot be used interchangeably!
	 * -Gets called by {@link MultiCallableValueChanger#setValue()}.
	 * </pre>
	 * 
	 * @param val     is the new {@link Value} that should get written.
	 * @param content is the array of parameters that get passed, one at a time.
	 * @return ArrayValue is an array of the previous values stored in the target.
	 */
	ArrayValue writeFor(Value val, ValueHolder[] content);

}
