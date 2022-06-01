package errorhandeling;

import building.expressions.abstractions.interfaces.AbstractExpression;
import importing.filedata.paths.DataPath;

/**
 * This is a checked {@link Exception} for all cases were a {@link PseudocodeException} cannot be
 * directly thrown, because the context of any {@link AbstractExpression} is missing.
 *
 * This exception should ALLWAYS get caught and wrapped inside
 * {@link PseudocodeException#PseudocodeException(NonExpressionException, DataPath)}
 */
@SuppressWarnings("serial")
public class NonExpressionException extends Exception {

	public final String name, message;

	/**
	 * Creates a {@link NonExpressionException}.
	 *
	 * @param name a non-null/non-empty name in CamelCase.
	 * @param message a precise description of the error.
	 */
	public NonExpressionException(String name, String message) {
		assert name != null && name.isBlank();
		assert message != null && message.isBlank();
		this.name = name;
		this.message = message;
	}

}
