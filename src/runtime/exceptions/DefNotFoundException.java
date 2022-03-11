package runtime.exceptions;

/** Gets thrown by {@link Scope#getFunc(String, int))}. */
@SuppressWarnings("serial")
public class DefNotFoundException extends AbstractRuntimeException {

	public DefNotFoundException(int line, String message) {
		super(line, message);
	}

}
