package exceptions;

/** Every type of Exception that gets caused by this Interpreter. */
@SuppressWarnings("serial")
public abstract class InterpretingException extends IllegalStateException {
	public InterpretingException(String message) {
		super(message);
	}
}
