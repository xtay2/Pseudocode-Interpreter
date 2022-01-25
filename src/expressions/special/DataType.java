package expressions.special;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum DataType {
	// Vartypes
	VAR("var"), TEXT("text"), BOOL("bool"), NUMBER("nr"),
	// Types of arrays:
	VAR_ARRAY("var[]"), BOOL_ARRAY("bool[]"), NUMBER_ARRAY("nr[]"), TEXT_ARRAY("text[]");

	public static boolean isArrayType(DataType type) {
		return type == VAR_ARRAY || type == NUMBER_ARRAY || type == TEXT_ARRAY || type == BOOL_ARRAY;
	}

	public static boolean isType(String s) {
		return stringToType(s) != null;
	}

	public static DataType stringToType(String s) {
		for (DataType t : DataType.values()) {
			if (t.name.equals(s.strip()))
				return t;
		}
		return null;
	}

	private final String name;

	DataType(String string) {
		name = string;
	}

	public String getName() {
		return name;
	}

}
