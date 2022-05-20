package building.expressions.abstractions.interfaces;

import building.types.specific.datatypes.DataType;
import misc.constants.TypeConstants;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;

/**
 * An Interface for everything that can get casted.
 *
 * @see Value
 */
public interface Castable {

	/**
	 * Cast this {@link Castable} to the passed {@link DataType}.
	 *
	 * @throws CastingException Can result in a {@link CastingException}, if the cast is not supported.
	 */
	public Value as(DataType t) throws CastingException;

	// CASTING-SHORTCUTS

	public default NumberValue asNr() {
		return (NumberValue) as(TypeConstants.NR);
	}

	public default IntValue asInt() {
		return (IntValue) as(TypeConstants.INT);
	}

	public default TextValue asText() {
		return (TextValue) as(TypeConstants.TEXT);
	}

	public default CharValue asChar() {
		return (CharValue) as(TypeConstants.CHAR);
	}

	public default BoolValue asBool() {
		return (BoolValue) as(TypeConstants.BOOL);
	}
}
