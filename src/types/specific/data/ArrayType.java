package types.specific.data;

import static types.specific.data.DataType.*;

import expressions.normal.BuilderExpression;
import types.SuperType;

public enum ArrayType implements ExpectedType {

	// Types of arrays:
	VAR_ARRAY(VAR), TEXT_ARRAY(TEXT), BOOL_ARRAY(BOOL), NUMBER_ARRAY(NUMBER), INT_ARRAY(INT),

	// Not implemented
	@Deprecated
	OBJECT_ARRAY(OBJECT);

	public final DataType dataType;

	ArrayType(DataType type) {
		this.dataType = type;
	}

	@Override
	public BuilderExpression create(String arg, int lineID) {
		if (!toString().equals(arg.strip()))
			return null;
		return new BuilderExpression(this);
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
}
