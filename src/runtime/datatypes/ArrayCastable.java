package runtime.datatypes;

import building.types.specific.datatypes.ArrayType;
import runtime.datatypes.array.ArrayValue;

public interface ArrayCastable {

	/**
	 * Casts any value to an array.
	 * 
	 * @param at is the array-type that the result should have.
	 */
	public ArrayValue asArray(ArrayType at);

}
