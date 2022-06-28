package building.expressions.abstractions.interfaces;

import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.expressions.possible.allocating.*;
import runtime.datatypes.*;

/**
 * Everything that gets its value changed by an {@link Allocating}-Expression.
 * 
 * @see Name
 * @see ArrayAccess
 */
public interface ValueChanger extends ValueHolder, NameHolder {
	
	/**
	 * Changes the value of the underlying {@link ValueHolder}.
	 * 
	 * @param val is the new {@link Value}.
	 * @return the old {@link Value}.
	 */
	public Value setValue(Value val);
	
}
