package building.expressions.abstractions.interfaces;

import static runtime.datatypes.numerical.ConceptualNrValue.NAN;

import building.types.specific.data.ArrayType;
import building.types.specific.data.DataType;
import building.types.specific.data.ExpectedType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.functional.DefValue;
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

	/** Returns the {@link ExpectedType} of this {@link Castable}. */
	public ExpectedType getType();

	/** Returns true if this {@link Castable} can get casted to the passed {@link ExpectedType}. */
	public boolean canCastTo(ExpectedType t);

	/**
	 * Cast this {@link Castable} to the passed {@link ExpectedType}.
	 * 
	 * @throws CastingException Can result in a {@link CastingException}, if the cast is not supported.
	 */
	public default Value as(ExpectedType t) throws CastingException {
		return switch (t) {
			case DataType d:
				yield switch (d) {
					case VAR -> getValue();
					case BOOL -> asBool();
					case NUMBER -> asNumber();
					case INT -> asInt();
					case TEXT -> asText();
					case CHAR -> asChar();
					case DEF -> asDef();
					case OBJECT -> throw new UnsupportedOperationException("Unsupported case: " + d);
				};
			case ArrayType a:
				yield switch (a) {
					case VAR_ARRAY -> asVarArray();
					case BOOL_ARRAY -> asBoolArray();
					case INT_ARRAY -> asIntArray();
					case NUMBER_ARRAY -> asNumberArray();
					case TEXT_ARRAY -> asTextArray();
					case CHAR_ARRAY -> asCharArray();
					case DEF_ARRAY -> asDefArray();
					case OBJECT_ARRAY -> throw new UnsupportedOperationException("Unsupported case: " + a);
				};
			default:
				throw new UnexpectedTypeError(t);
		};
	}

	public default BoolValue asBool() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a BoolValue.");
	}

	/** Everything can be casted to a number or NaN */
	public default NumberValue asNumber() {
		return NAN;
	}

	public default IntValue asInt() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a IntValue.");
	}

	/** Everything should have a text-representation. */
	public TextValue asText();

	public default CharValue asChar() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a CharValue.");
	}

	public default DefValue asDef() {
		throw new CastingException("A " + getType() + " cannot be casted to a def.");
	}

	/** Returns a characterwise textrepresentation for default. */
	public default ArrayValue asVarArray() {
		return asText().asVarArray();
	}

	public default ArrayValue asBoolArray() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a BoolArray.");
	}

	public default ArrayValue asNumberArray() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a NumberArray.");
	}

	public default ArrayValue asIntArray() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a IntArray.");
	}

	public default ArrayValue asTextArray() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a TextArray.");
	}

	/** Returns a characterwise textrepresentation for default. */
	public default ArrayValue asCharArray() {
		return asText().asCharArray();
	}

	public default ArrayValue asDefArray() throws CastingException {
		throw new CastingException("A " + getType() + " cannot be casted to a DefArray.");
	}
}
