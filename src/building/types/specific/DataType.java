package building.types.specific;

import static building.types.abstractions.SuperType.AFTER_VALUE_TYPE;
import static building.types.abstractions.SuperType.ASSIGNMENT_TYPE;
import static building.types.specific.DynamicType.NAME;

import java.util.Arrays;

import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.object.NullValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum DataType implements SpecificType {

	// Vartypes
	VAR("var"), TEXT("text"), CHAR("char"), BOOL("bool"), NUMBER("nr"), INT("int"),

	// Not implemented

	@Deprecated
	OBJECT("obj"),

	// Vartypes
	VAR_ARRAY(VAR), TEXT_ARRAY(TEXT), CHAR_ARRAY(CHAR), BOOL_ARRAY(BOOL), NUMBER_ARRAY(NUMBER), INT_ARRAY(INT),

	// Not implemented

	@Deprecated
	OBJECT_ARRAY(OBJECT);

	final String symbol;

	private final DataType match;

	DataType(String type) {
		this.symbol = type;
		match = null;
	}

	DataType(DataType type) {
		this.symbol = type.symbol + "[]";
		match = type;
	}

	public Value stdVal() {
		return switch (this) {
			case VAR:
				throw new IllegalCodeFormatException("A declaration that uses var, has to get initialised with a value.");
			case OBJECT:
				yield NullValue.NULL;
			case BOOL:
				yield BoolValue.FALSE;
			case INT, NUMBER:
				yield NumberValue.ZERO;
			case TEXT:
				yield new TextValue("");
			case CHAR:
				yield new CharValue(' ');
			default:
				if (isArray())
					yield new ArrayValue(this);
				throw new AssertionError();
		};
	}

	@Override
	public boolean is(AbstractType other) {
		return SpecificType.super.is(other) || this == INT && other == NUMBER || this == CHAR && other == TEXT;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return new AbstractType[] { ASSIGNMENT_TYPE, AFTER_VALUE_TYPE, NAME };
	}

	@Override
	public String toString() {
		return symbol;
	}

	/** Returns all array-values. */
	public static AbstractType[] arrayValues() {
		Object[] o = Arrays.stream(values()).filter(e -> e.isArray()).toArray();
		return Arrays.copyOf(o, o.length, AbstractType[].class);
	}

	public boolean isArray() {
		return match != null;
	}

	public DataType toDataType() {
		return isArray() ? match : this;
	}

	public DataType toArrayType() {
		return isArray() ? this : Arrays.stream(values()).filter(e -> e.match == this).findFirst().get();
	}

}
