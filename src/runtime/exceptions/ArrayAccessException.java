package runtime.exceptions;

/** Gets thrown when theres an illegal array access at runtime. Example a[-1] */
@SuppressWarnings("serial")
public class ArrayAccessException extends AbstractRuntimeException {

	public ArrayAccessException(int line, String message) {
		super(line, message);
	}

}
