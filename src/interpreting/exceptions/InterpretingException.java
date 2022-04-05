package interpreting.exceptions;

/** Every type of Exception that gets caused by this Interpreter. */
@SuppressWarnings("serial")
public abstract class InterpretingException extends IllegalStateException {

	/**
	 * @param line is the original line of code which produced this error.
	 */
	public InterpretingException(int line, String message) {
		super(line == -1 ? message : "Line " + line + ": " + message);
	}
}
