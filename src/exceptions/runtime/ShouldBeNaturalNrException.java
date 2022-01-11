package exceptions.runtime;

/**
 * Gets thrown when a nr should be a positive (0 inclusive) integer.
 */
@SuppressWarnings("serial")
public class ShouldBeNaturalNrException extends AbstractRuntimeException {

	public ShouldBeNaturalNrException(int line, String message) {
		super(line, message);
	}
	
}
