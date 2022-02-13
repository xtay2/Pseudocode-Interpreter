package exceptions.parsing;

import expressions.normal.flag.Flaggable;

/**
 * Gets thrown when theres a wrong flag standing in front of a {@link Flaggable}.
 */
@SuppressWarnings("serial")
public class UnexpectedFlagException extends IllegalCodeFormatException{

	public UnexpectedFlagException(int line, String message) {
		super(line, message);
	}

}
