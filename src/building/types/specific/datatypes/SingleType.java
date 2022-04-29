package building.types.specific.datatypes;

import static building.types.abstractions.SuperType.AFTER_VALUE_TYPE;
import static building.types.abstractions.SuperType.ASSIGNMENT_TYPE;
import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.BuilderType.MAYBE;
import static building.types.specific.DynamicType.NAME;
import static runtime.datatypes.MaybeValue.NULL;

import building.types.abstractions.AbstractType;
import building.types.abstractions.SuperType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.DeclarationException;

/** The Type of a Variable. If Type == null, the var has no specific type. */
public enum SingleType implements DataType {

	// Vartypes
	VAR("var"), TEXT("text"), CHAR("char"), BOOL("bool"), NUMBER("nr"), INT("int"),

	// Not implemented

	@Deprecated
	OBJECT("obj");

	final String symbol;

	private SingleType(String type) {
		this.symbol = type;
	}

	@Override
	public Value stdVal(boolean allowsNull) {
		return switch (this) {
			case VAR:
				if (allowsNull)
					yield NULL;
				else
					throw new DeclarationException(-1, "A variable with the var-type has to be initialised at its declaration.");
			case BOOL:
				yield BoolValue.FALSE;
			case INT, NUMBER:
				yield NumberValue.ZERO;
			case TEXT:
				yield new TextValue("");
			case CHAR:
				yield new CharValue(' ');
			case OBJECT:
				throw new UnsupportedOperationException("Unimplemented case: " + this);
		};
	}

	@Override
	public boolean is(AbstractType other) {
		return (this == INT && other == NUMBER) || (this == CHAR && other == TEXT) || other == SuperType.DATA_TYPE;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return new AbstractType[] { MAYBE, ARRAY_START, ASSIGNMENT_TYPE, AFTER_VALUE_TYPE, NAME };
	}

	@Override
	public String toString() {
		return symbol;
	}

}
