package runtime.exceptions;

import building.types.specific.datatypes.DataType;
import runtime.datatypes.Value;

/**
 * Gets thrown whenever there is an attempt made to set a variable with a {@link Value} that doesn't
 * match a certain {@link DataType}.
 *
 * <pre>
 * Examples:
 * -Variables
 * -Parameters
 * -Return-Values
 * -Writing into an array
 * </pre>
 */
@SuppressWarnings("serial")
public class UnexpectedDataType extends AbstractRuntimeException {

	public UnexpectedDataType(int line, String message) {
		super(line, message);
	}
}
