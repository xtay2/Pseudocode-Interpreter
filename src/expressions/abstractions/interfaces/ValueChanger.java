package expressions.abstractions.interfaces;

import datatypes.Value;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;

/**
 * Everything that can change its value.
 * @see Name
 * @see ArrayAccess
 */
public interface ValueChanger extends ValueHolder {

	public void setValue(Value val);
	
}
