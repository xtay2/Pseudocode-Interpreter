package errorhandeling;

import importing.filedata.paths.DataPath;
import importing.filedata.paths.FilePath;
import interpreting.program.Program;
import launching.Main;
import misc.helper.StringHelper;

/**
 * These errors get throw throughout the whole project. They are caught in th main-function
 */
@SuppressWarnings("serial")
public class PseudocodeException extends RuntimeException {

	private final String name, message, line;
	private final FilePath location;

	/**
	 * Create a {@link PseudocodeException} whenever something goes wrong.
	 *
	 * @param name is the name of the exception. It should be in CamelCase.
	 * @param message is a precise description of what exactly happened.
	 * @param line is the line of code that lead to this exception. It can be obtained by calling
	 * {@link Program#find(DataPath)}. The relevant part should get underlined by calling
	 * {@link StringHelper#pointUnderline(String, int, int)}.
	 * @param location is the location of the code-line.
	 */
	public PseudocodeException(String name, String message, String line, FilePath location) {
		assert name != null && !name.isBlank() : "A PseudocodeException exception has to have a name.";
		assert message != null && !message.isBlank() : "A PseudocodeException exception has to have a message.";
		assert location != null : "A PseudocodeException has to have a location.";
		this.name = name.endsWith("Exception") ? name : name + "Exception";
		this.message = message;
		this.location = location;
		if (line == null) {
			line = "";
			if (location instanceof DataPath dp && Main.PROGRAM.isConstructed())
				line = Main.PROGRAM.find(dp);
		}
		this.line = line;
	}

	/** A {@link PseudocodeException} without a specially formatted line. */
	public PseudocodeException(String name, String message, FilePath location) {
		this(name, message, null, location);
	}

	/** Java {@link Exception} Wrapper. */
	public PseudocodeException(Exception e, FilePath location) {
		this(e.getClass().getSimpleName(), e.getMessage(), null, location);
		initCause(e);
	}

	/** Inner {@link NonExpressionException} Wrapper. */
	public PseudocodeException(NonExpressionException e, DataPath location) {
		this(e.name, e.getMessage(), null, location);
		initCause(e);
	}

	@Override
	public String toString() {
		String arcticle = name.matches("[aeiouAEIOU].*") ? "An" : "A";
		return arcticle + " " + name + " was thrown at " + location.toString() + ".\n"//
				+ message + (line.isBlank() ? "" : "\n" + line);
	}
}
