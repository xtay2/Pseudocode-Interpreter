package runtime.exceptions;

import building.expressions.abstractions.interfaces.Castable;
import building.types.specific.datatypes.DataType;

/**
 * Gets thrown, when the user tries to perform an invalid cast, such as: "Hallo" -> Nr
 */
@SuppressWarnings("serial")
public class CastingException extends AbstractRuntimeException {

	public CastingException(int line, String message) {
		super(line, message);

	}

	/**
	 * Should only be used when theres no definite way of finding the origin (line).
	 */
	public CastingException(String message) {
		super(-1, message);
	}

	public CastingException(Castable targetVal, DataType targetType) {
		super(-1, "Cannot cast " + targetVal + " to " + targetType + ".");
	}
}
