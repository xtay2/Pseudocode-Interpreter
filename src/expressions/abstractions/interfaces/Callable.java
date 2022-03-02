package expressions.abstractions.interfaces;

import datatypes.DefValue;
import datatypes.Value;
import expressions.main.functions.Returnable;

/**
 * Everything that can be called and provides a return-value.
 * 
 * @see Returnable
 * @see DefValue
 */
public interface Callable {

	/**
	 * Calls any {@link Callable} with parameters.
	 * 
	 * @return the return-value or null if none exists.
	 */
	public Value call(ValueHolder... params);

}
