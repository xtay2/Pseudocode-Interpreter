package exceptions.parsing;

import exceptions.InterpretingException;

/**
 * Super gets thrown when the Interpreter finds ill formatted code and doesnt
 * know how to read it.
 */
@SuppressWarnings("serial")
public class IllegalCodeFormatException extends InterpretingException {

	public IllegalCodeFormatException(String message) {
		super(message);
	}

}
