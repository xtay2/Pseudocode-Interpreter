package types.specific;

import expressions.abstractions.Expression;
import expressions.normal.ExpectedType;
import types.AbstractType;
import types.SuperType;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum DataType implements AbstractType {

	// Vartypes
	VAR("var"), TEXT("text"), BOOL("bool"), NUMBER("nr"), INT("int"),

	// Types of arrays:
	VAR_ARRAY("var[]"), TEXT_ARRAY("text[]"), BOOL_ARRAY("bool[]"), NUMBER_ARRAY("nr[]"), INT_ARRAY("int[]"),

	// Not implemented
	@Deprecated
	OBJECT("obj"),

	@Deprecated
	OBJECT_ARRAY("obj[]");

	/**
	 * Returns true if the passed type is an ArrayType.
	 */
	public static boolean isArrayType(AbstractType type) {
		return type instanceof DataType d
				&& (d == VAR_ARRAY || d == NUMBER_ARRAY || d == TEXT_ARRAY || d == BOOL_ARRAY || d == INT_ARRAY || d == OBJECT_ARRAY);
	}

	private final String type;

	DataType(String type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return type;
	}

	@Override
	public Expression create(String arg, int lineID) {
		if (!type.equals(arg.strip()))
			return null;
		return new ExpectedType(this, lineID);
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.DATA_TYPE;
	}

	/** Checks, if the passed {@link String} is a {@link DataType}. */
	public static boolean isType(String arg) {
		for (DataType t : DataType.values()) {
			if (t.type.equals(arg))
				return true;
		}
		return false;
	}
}
