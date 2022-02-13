package expressions.abstractions;

import datatypes.Value;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;

/**
 * Everything that can change its value.
 * @see {@link Name}
 * @see {@link ArrayAccess}
 */
public interface ValueChanger extends ValueHolder {

	public void setValue(Value val);
	
}
