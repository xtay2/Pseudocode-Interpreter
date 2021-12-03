package exceptions;

/**
 * Gets thrown, when the user tries to perform an invalid cast,
 * such as: "Hallo" -> Int
 */
public class CastingException extends IllegalArgumentException {

	private static final long serialVersionUID = 1L;

	public CastingException(String errorMsg) {
		super(errorMsg);
	}

}
