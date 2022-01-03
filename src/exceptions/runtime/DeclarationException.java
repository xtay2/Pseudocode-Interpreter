package exceptions.runtime;

/**
 * Gets thrown when something went wrong while definining a function or variable
 * at runtime.
 */
@SuppressWarnings("serial")
public class DeclarationException extends AbstractRuntimeException {

	public DeclarationException(String message) {
		super(message);
	}

}
