package exceptions.parsing;

import exceptions.InterpretingException;

/**
 * Super gets thrown when the Interpreter finds ill formatted code and doesnt
 * know how to read it.
 */
@SuppressWarnings("serial")
public class IllegalCodeFormatException extends InterpretingException {

	public IllegalCodeFormatException(int line, String message) {
		super(line, message);
	}

	/**
	 * Should only be used when theres no definite way of finding the origin (line).
	 */
	public IllegalCodeFormatException(String message) {
		super(-1, message);
	}

}
