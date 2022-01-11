package exceptions.runtime;

/** Gets thrown when a variable or funtion which isn't defined, gets called. */
@SuppressWarnings("serial")
public class IllegalCallException extends AbstractRuntimeException {

	public IllegalCallException(int line, String message) {
		super(line, message);
	}

}
