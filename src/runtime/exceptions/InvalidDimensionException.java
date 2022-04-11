package runtime.exceptions;

/**
 * Gets thrown when an array is treated/expected with an incompatible amount of dimensions.
 */
@SuppressWarnings("serial")
public class InvalidDimensionException extends AbstractRuntimeException {

	public InvalidDimensionException(int line, String message) {
		super(line, message);
	}

	public InvalidDimensionException(String message) {
		super(-1, message);
	}

}
