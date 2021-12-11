package expressions.special;

/** The Type of a Variable. If Type == null, the var has no specific type.*/
public enum Type {

	VAR("var"), NUMBER("nr"), TEXT("text"), BOOL("bool"), 
	
	//Types of arrays:
	NUMBER_ARRAY("nr[]"), TEXT_ARRAY("text[]"), BOOL_ARRAY("bool[]"), VAR_ARRAY("var[]");

	private final String name;

	public String getName() {
		return name;
	}

	Type(String string) {
		name = string;
	}

	public static Type stringToType(String s) {
		for (Type t : Type.values()) {
			if (t.name.equals(s.strip()))
				return t;	
		}
		return null;
	}

	public static boolean isType(String s) {
		return stringToType(s) != null;
	}

}
