package expressions.special;

import datatypes.Value;
import expressions.normal.array.ArrayAccess;

/**
 * An interface for all ValueHolders that can also change the value.
 * 
 * Examples:
 * 
 * @see {@link Name}
 * @see {@link Call}
 * @see {@link Literal}
 * @see {@link ArrayAccess}
 */
public interface ValueHolder {

	public Value getValue();

}
