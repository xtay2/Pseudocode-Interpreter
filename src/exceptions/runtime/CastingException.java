package exceptions.runtime;

/**
 * Gets thrown, when the user tries to perform an invalid cast,
 * such as: "Hallo" -> Nr
 */
@SuppressWarnings("serial")
public class CastingException extends AbstractRuntimeException {

	public CastingException(String message) {
		super(message);
	}

}
