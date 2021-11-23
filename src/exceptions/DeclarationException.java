package exceptions;

/**
 * Gets thrown when something went wrong while definining a function or
 * variable.
 */
public class DeclarationException extends IllegalArgumentException {

	private static final long serialVersionUID = 1L;
	
	public DeclarationException(String errorMsg) {
		super(errorMsg);
	}
}
