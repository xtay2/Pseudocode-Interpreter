package errorhandeling;

import launching.*;

/**
 * This is a {@link RuntimeException} that gets thrown when absolutely no information about code is
 * available or necessary. It gets used to wrap checked Exceptions.
 */
public class InitException extends RuntimeException {

	public final String name;

	/**
	 * Creates an {@link InitException} as a named wrapper for a checked exception.
	 *
	 * @param description is a detailed description of what went wrong because of the cause.
	 */
	public InitException(String name, String description, Exception cause) {
		super(description, cause);
		this.name = name;
		assert !Main.PROGRAM.isConstructed() : "Its way too late to throw this exception. The program is allready initialized.";
	}

	/**
	 * Wraps a checked {@link Exception}.
	 *
	 * @param description is a detailed description of what went wrong because of the cause.
	 */
	public InitException(String description, Exception cause) {
		this(cause.getClass().getSimpleName(), description, cause);
	}

}