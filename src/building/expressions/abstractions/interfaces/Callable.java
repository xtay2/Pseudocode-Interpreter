package building.expressions.abstractions.interfaces;

import building.expressions.main.functions.Definition;
import runtime.datatypes.Value;

/**
 * Everything that can be called and provides a return-value.
 * 
 * @see Definition
 * @see Name
 */
public interface Callable {

	/**
	 * Calls any {@link Callable} with parameters.
	 * 
	 * @return the return-value or null if none exists.
	 */
	public Value call(ValueHolder... params);

}
