package building.types.specific.datatypes;

import building.types.abstractions.SpecificType;
import runtime.datatypes.Value;

/**
 * @see SingleType
 * @see ArrayType
 */
public interface DataType extends SpecificType {

	/** Returns the value that a variable gets when it has none at the declaration. */
	public Value stdVal();
}
