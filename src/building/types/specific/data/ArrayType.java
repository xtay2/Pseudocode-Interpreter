package building.types.specific.data;

import static building.types.specific.data.DataType.*;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.SuperType;
import runtime.datatypes.array.ArrayValue;

public enum ArrayType implements ExpectedType {

	// Types of arrays:
	VAR_ARRAY(VAR), TEXT_ARRAY(TEXT), BOOL_ARRAY(BOOL), NUMBER_ARRAY(NUMBER), INT_ARRAY(INT),

	// Not implemented
	DEF_ARRAY(DEF),

	@Deprecated
	OBJECT_ARRAY(OBJECT);

	public final DataType dataType;

	private ArrayType(DataType type) {
		this.dataType = type;
	}

	/** Checks, if the passed {@link String} is a {@link ArrayType}. */
	public static boolean isType(String arg) {
		for (ArrayType t : values()) {
			if (t.toString().equals(arg))
				return true;
		}
		return false;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.ARRAY_TYPE;
	}

	@Override
	public String toString() {
		return dataType.toString() + "[]";
	}

	/**
	 * Returns an empty array of this type.
	 */
	@Override
	public ValueHolder stdVal() {
		return new ArrayValue(this);
	}
}
