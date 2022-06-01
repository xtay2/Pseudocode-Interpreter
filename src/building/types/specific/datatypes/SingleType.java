package building.types.specific.datatypes;

import static building.types.abstractions.SuperType.AFTER_VALUE_TYPE;
import static building.types.abstractions.SuperType.ASSIGNMENT_TYPE;
import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.BuilderType.MAYBE;
import static building.types.specific.DynamicType.NAME;
import static runtime.datatypes.BoolValue.FALSE;
import static runtime.datatypes.MaybeValue.NULL;
import static runtime.datatypes.numerical.NumberValue.ZERO;

import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import errorhandeling.NonExpressionException;
import runtime.datatypes.Value;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;

/**
 * The base type for a {@link DataType}.
 */
public enum SingleType implements SpecificType {

	// Vartypes
	VAR("var"), TEXT("text"), CHAR("char"), BOOL("bool"), NR("nr"), INT("int");

	public final String txt;

	private SingleType(String txt) {
		this.txt = txt;
	}

	/**
	 * Returns the default-value which can vary dependent null-allowance
	 *
	 * @param allowsNull
	 * @throws NonExpressionException for when this type doesn't support a stdVal.
	 */
	public Value stdVal(boolean allowsNull) throws NonExpressionException {
		return switch (this) {
			case VAR:
				if (allowsNull)
					yield NULL;
				else {
					throw new NonExpressionException("StandardValue",
							"A variable with the var-type has to be initialised at its declaration.");
				}
			case BOOL:
				yield FALSE;
			case INT, NR:
				yield ZERO;
			case TEXT:
				yield new TextValue("");
			case CHAR:
				yield new CharValue(' ');
		};
	}

	/**
	 * Returns true if this is either equal to the passed {@link SingleType} or a constraint, like
	 * {@link #CHAR} for {@link #TEXT}.
	 */
	public boolean is(SingleType other) {
		return (this == other) || other == VAR || (this == INT && other == NR) || (this == CHAR && other == TEXT);
	}

	@Override
	public AbstractType[] abstractExpected() {
		return new AbstractType[] { MAYBE, ARRAY_START, ASSIGNMENT_TYPE, AFTER_VALUE_TYPE, NAME };
	}

	@Override
	public String toString() {
		return txt;
	}
}