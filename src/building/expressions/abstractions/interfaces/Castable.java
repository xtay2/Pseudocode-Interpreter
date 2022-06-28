package building.expressions.abstractions.interfaces;

import building.types.specific.datatypes.*;
import errorhandeling.*;
import misc.constants.*;
import runtime.datatypes.*;
import runtime.datatypes.numerical.*;
import runtime.datatypes.textual.*;

/**
 * An Interface for everything that can get casted.
 *
 * @see Value
 */
public interface Castable {
	
	/**
	 * Cast this {@link Castable} to the passed {@link DataType}.
	 *
	 * @throws NonExpressionException, if the cast is not supported.
	 */
	public Value as(DataType t) throws NonExpressionException;
	
	// CASTING-SHORTCUTS
	
	default NumberValue asNr() throws NonExpressionException {
		return (NumberValue) as(TypeConstants.NR);
	}
	
	default IntValue asInt() throws NonExpressionException {
		return (IntValue) as(TypeConstants.INT);
	}
	
	default TextValue asText() {
		try {
			return (TextValue) as(TypeConstants.TEXT);
		} catch (NonExpressionException e) {
			throw new AssertionError("It should allways be possible to cast to text.", e);
		}
	}
	
	default CharValue asChar() throws NonExpressionException {
		return (CharValue) as(TypeConstants.CHAR);
	}
	
	default BoolValue asBool() throws NonExpressionException {
		return (BoolValue) as(TypeConstants.BOOL);
	}
}
