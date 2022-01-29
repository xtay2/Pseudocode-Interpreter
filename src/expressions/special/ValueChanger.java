package expressions.special;

import datatypes.Value;
import expressions.normal.Name;
import expressions.normal.array.ArrayAccess;

/**
 * Everything that can change its value.
 * @see {@link Name}
 * @see {@link ArrayAccess}
 */
public interface ValueChanger extends ValueHolder {

	public void setValue(Value val);
	
}
