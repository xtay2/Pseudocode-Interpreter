package exceptions.runtime;

/**
 * Gets thrown when trying to operate on a castable with an unexpected type.
 * 
 * Should be used as a kind of Assertion.
 */
@SuppressWarnings("serial")
public class UnexpectedTypeError extends AssertionError {

	public UnexpectedTypeError(String message) {
		super(message);
		System.err.println("This shouldn't get thrown. Maybe there is a bug in the Interpreter.");
	}

}
