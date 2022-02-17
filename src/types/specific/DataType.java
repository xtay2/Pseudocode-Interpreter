package types.specific;

import types.SpecificType;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum DataType implements SpecificType {

	// Vartypes
	VAR("var"), TEXT("text"), BOOL("bool"), NUMBER("nr"), INT("int"),

	// Types of arrays:
	VAR_ARRAY("var[]"), TEXT_ARRAY("text[]"), BOOL_ARRAY("bool[]"), NUMBER_ARRAY("nr[]"), INT_ARRAY("int[]"),

	// Not implemented
	@Deprecated
	OBJECT("obj"),

	@Deprecated
	OBJECT_ARRAY("obj[]");

	private final String type;

	DataType(String type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return type;
	}

	// STATIC FUNCTIONS

	public static boolean isArrayType(DataType type) {
		return type == VAR_ARRAY || type == NUMBER_ARRAY || type == TEXT_ARRAY || type == BOOL_ARRAY || type == INT_ARRAY;
	}

	public static boolean isType(String s) {
		return stringToType(s) != null;
	}

	public static DataType stringToType(String s) {
		for (DataType t : DataType.values()) {
			if (t.type.equals(s.strip()))
				return t;
		}
		return null;
	}
}
