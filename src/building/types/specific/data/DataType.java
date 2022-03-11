package building.types.specific.data;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.BuilderExpression;
import building.types.SuperType;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.BoolValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.object.NullValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum DataType implements ExpectedType {

	// Vartypes
	VAR("var"), TEXT("text"), CHAR("char"), BOOL("bool"), NUMBER("nr"), INT("int"), DEF("def"),

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
		return new BuilderExpression(lineID, this);
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

	@Override
	public ValueHolder stdVal() {
		return switch (this) {
			case VAR, OBJECT -> NullValue.NULL;
			case BOOL -> BoolValue.FALSE;
			case INT, NUMBER -> NumberValue.ZERO;
			case TEXT -> new TextValue("");
			case CHAR -> new CharValue(' ');
			case DEF -> throw new IllegalCodeFormatException("A def has to get initialised at declaration.");
		};
	}
}
