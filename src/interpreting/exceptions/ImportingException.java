package interpreting.exceptions;


@SuppressWarnings("serial")
public class ImportingException extends IllegalCodeFormatException {

	public ImportingException(int line, String message) {
		super(line, message);
	}

}
