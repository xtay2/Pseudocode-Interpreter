package types.specific.data;

import expressions.normal.BuilderExpression;
import types.SuperType;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum DataType implements ExpectedType {

	// Vartypes
	VAR("var"), TEXT("text"), BOOL("bool"), NUMBER("nr"), INT("int"),

	// Not implemented
	@Deprecated
	OBJECT("obj");

	private final String type;

	DataType(String type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return type;
	}

	@Override
	public BuilderExpression create(String arg, int lineID) {
		if (!type.equals(arg.strip()))
			return null;
		return new BuilderExpression(this);
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.DATA_TYPE;
	}

	/** Checks, if the passed {@link String} is a {@link DataType}. */
	public static boolean isType(String arg) {
		for (DataType t : values()) {
			if (t.type.equals(arg))
				return true;
		}
		return false;
	}
}
