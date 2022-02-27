package expressions.abstractions.interfaces;

import datatypes.Value;
import expressions.main.functions.Returnable;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;

/**
 * Everything that can change its value.
 * 
 * @see Name
 * @see ArrayAccess
 * @see Returnable
 */
public interface ValueChanger extends ValueHolder, NameHolder {

	public void setValue(Value val);

}
