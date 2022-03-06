package runtime.exceptions;

/** Gets thrown when something gets wrong while returning a value. */
@SuppressWarnings("serial")
public class IllegalReturnException extends AbstractRuntimeException {

	public IllegalReturnException(int line, String message) {
		super(line, message);
	}

}
