package building.expressions.abstractions.interfaces;

import static building.types.specific.datatypes.ArrayType.*;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;

import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.UnexpectedTypeError;

/**
 * An Interface for everything that can get casted.
 * 
 * @see Value
 */
public interface Castable extends ValueHolder {

	/** Returns the {@link DataType} of this {@link Castable}. */
	public DataType getType();

	/** Returns true if this {@link Castable} can get casted to the passed {@link DataType}. */
	public boolean canCastTo(SingleType t);

	/** Tells, if this Value can always be safely casted to the suggested {@link ArrayType}. */
	public boolean canCastTo(ArrayType type);

	/**
	 * Cast this {@link Castable} to the passed {@link DataType}.
	 * 
	 * @throws CastingException Can result in a {@link CastingException}, if the cast is not supported.
	 */
	public default Value as(DataType t) throws CastingException {
		if (t.equals(getType()))
			return getValue();
		if (t instanceof SingleType st) {
			return switch (st) {
				case VAR -> getValue();
				case BOOL -> asBool();
				case NUMBER -> asNumber();
				case INT -> asInt();
				case TEXT -> asText();
				case CHAR -> asChar();
				case OBJECT -> throw new UnsupportedOperationException("Unsupported case: " + t);
			};
		}
		if (t instanceof ArrayType at) {
			if (at.equals(VAR_ARRAY))
				return asVarArray();
			if (at.dimensions != 1)
				throw new CastingException(
						"Its only possible to cast to one-dimensional arrays. Tried to cast " + getType() + " to: " + at);
			if (at.equals(BOOL_ARRAY))
				return asBoolArray();
			if (at.equals(INT_ARRAY))
				return asIntArray();
			if (at.equals(NUMBER_ARRAY))
				return asNumberArray();
			if (at.equals(TEXT_ARRAY))
				return asTextArray();
			if (at.equals(CHAR_ARRAY))
				return asCharArray();
		}
		throw new UnexpectedTypeError(t);
	}

	public default BoolValue asBool() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a BoolValue.");
	}

	/** Everything can be casted to a number or NaN */
	public default NumberValue asNumber() {
		return NAN;
	}

	public default IntValue asInt() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a IntValue.");
	}

	/** Everything should have a text-representation. */
	public TextValue asText();

	public default CharValue asChar() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a CharValue.");
	}

	/** Returns a characterwise textrepresentation for default. */
	public default ArrayValue asVarArray() {
		return asText().asVarArray();
	}

	public default ArrayValue asBoolArray() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a BoolArray.");
	}

	public default ArrayValue asNumberArray() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a NumberArray.");
	}

	public default ArrayValue asIntArray() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a IntArray.");
	}

	public default ArrayValue asTextArray() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a TextArray.");
	}

	/** Returns a characterwise textrepresentation for default. */
	public default ArrayValue asCharArray() {
		return asText().asCharArray();
	}

	public default ArrayValue asDefArray() throws CastingException {
		throw new CastingException(getType() + " cannot be casted to a DefArray.");
	}
}
