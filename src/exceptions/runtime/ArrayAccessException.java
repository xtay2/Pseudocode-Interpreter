package exceptions.runtime;

/** Gets thrown when theres an illegal array access at runtime. Example a[-1] */
@SuppressWarnings("serial")
public class ArrayAccessException extends AbstractRuntimeException {

	public ArrayAccessException(String message) {
		super(message);
	}

}
