package exceptions.runtime;

import exceptions.InterpretingException;

/**
 * Super class for all Exceptions that occur after the code is passed to
 * expressions.
 */
@SuppressWarnings("serial")
public abstract class AbstractRuntimeException extends InterpretingException {

	public AbstractRuntimeException(String message) {
		super(message);
	}

}
