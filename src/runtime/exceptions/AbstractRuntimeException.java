package runtime.exceptions;

import interpreting.exceptions.InterpretingException;

/**
 * Super class for all Exceptions that occur after the code is passed to
 * expressions.
 */
@SuppressWarnings("serial")
public abstract class AbstractRuntimeException extends InterpretingException {

	AbstractRuntimeException(int line, String message) {
		super(line, message);
	}

}
