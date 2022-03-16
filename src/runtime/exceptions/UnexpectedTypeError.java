package runtime.exceptions;

import building.types.abstractions.AbstractType;

/**
 * Gets thrown when trying to operate on a castable with an unexpected type.
 * 
 * Should be used as a kind of Assertion.
 */
@SuppressWarnings("serial")
public class UnexpectedTypeError extends AssertionError {

	public UnexpectedTypeError(AbstractType type) {
		this(-1, type);
	}

	public UnexpectedTypeError(int orgLine, AbstractType type) {
		super("Unexpected type " + type + (orgLine != -1 ? "in line " + orgLine : ""));
		System.err.println("This shouldn't get thrown. Maybe there is a bug in the Interpreter.");
	}

}
