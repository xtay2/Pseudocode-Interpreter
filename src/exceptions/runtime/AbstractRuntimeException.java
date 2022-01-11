package exceptions.runtime;

import exceptions.InterpretingException;

/**
 * Super class for all Exceptions that occur after the code is passed to
 * expressions.
 */
@SuppressWarnings("serial")
public abstract class AbstractRuntimeException extends InterpretingException {

	public AbstractRuntimeException(int line, String message) {
		super(line, message);
	}

}
