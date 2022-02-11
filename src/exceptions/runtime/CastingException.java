package exceptions.runtime;

/**
 * Gets thrown, when the user tries to perform an invalid cast, such as: "Hallo"
 * -> Nr
 */
@SuppressWarnings("serial")
public class CastingException extends AbstractRuntimeException {

	public CastingException(int line, String message) {
		super(line, message);
		
	}

	/**
	 * Should only be used when theres no definite way of finding the origin (line).
	 */
	public CastingException(String message) {
		super(-1, message);
	}
}
