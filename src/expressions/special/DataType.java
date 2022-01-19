package expressions.special;

/** The Type of a Variable. If Type == null, the var has no specific type.*/
public enum DataType {

	BOOL("bool"), BOOL_ARRAY("bool[]"), NUMBER("nr"), //Types of arrays:
	NUMBER_ARRAY("nr[]"), 
	
	TEXT("text"), TEXT_ARRAY("text[]"), VAR("var"), VAR_ARRAY("var[]");

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
