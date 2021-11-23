package programreader.expressions.special;

public enum Type {

	NUMBER("nr"), TEXT("text"), BOOL("bool");

	private final String name;
	
	public String getName() {
		return name;
	}
	
	Type(String string) {
		name = string;
	}

	public static Type stringToType(String s) {
		for (Type t : Type.values()) {
			if (t.getName().equals(s))
				return t;
		}
		return null;
	}
	
	public static boolean isType(String s) {
		return stringToType(s) != null;
	}

}
