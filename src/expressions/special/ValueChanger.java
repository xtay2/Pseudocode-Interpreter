package expressions.special;

import datatypes.Value;
import expressions.normal.Variable;
import expressions.normal.array.ArrayAccess;

/**
 * An interface for all ValueHolders that can also change the value.
 * 
 * @see {@link Variable}
 * @see {@link ArrayAccess}
 */
public interface ValueChanger extends ValueHolder {

	public void setValue(Value val);

}
