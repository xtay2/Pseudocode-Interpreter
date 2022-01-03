package exceptions.runtime;

/** Gets thrown when something gets wrong while returning a value. */
@SuppressWarnings("serial")
public class IllegalReturnException extends AbstractRuntimeException {

	public IllegalReturnException(String message) {
		super(message);
	}

}
