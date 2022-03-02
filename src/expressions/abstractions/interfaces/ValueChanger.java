package expressions.abstractions.interfaces;

import datatypes.Value;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;
import expressions.possible.assigning.Allocating;

/**
 * Everything that gets its value changed by an {@link Allocating}-Expression.
 * 
 * @see Name
 * @see ArrayAccess
 */
public interface ValueChanger extends ValueHolder, NameHolder {

	public void setValue(Value val);

}
