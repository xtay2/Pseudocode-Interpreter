package building.expressions.abstractions.interfaces;

import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.expressions.possible.allocating.Allocating;
import runtime.datatypes.Value;

/**
 * Everything that gets its value changed by an {@link Allocating}-Expression.
 * 
 * @see Name
 * @see ArrayAccess
 */
public interface ValueChanger extends ValueHolder, NameHolder {

	public void setValue(Value val);

}
