package building.expressions.possible.multicall;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import runtime.datatypes.*;

public interface MultiCallableValueHolder extends MultiCallable, ValueHolder {
	
	/**
	 * Should call {@link MultiCallable#getValue()} once for every element in content.
	 * 
	 * <pre>
	 * -Is similar to {@link MainExpression#getValue()} but cannot be used interchangeably!
	 * -Gets called by {@link MultiCallableValueHolder#getValue()}.
	 * </pre>
	 * 
	 * @param content is the array of parameters that get passed, one at a time.
	 * @return Value is an optional return-value that gets passed by a {@link ValueHolder}.
	 */
	Value executeFor(ValueHolder[] content);
}
