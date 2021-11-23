package programreader.finder;

public class NameFinder {

	/**
	 * Arg is valid name if alphanumerical with underscores
	 */
	public static boolean isName(String arg) {
		return arg.matches("([a-z]|[A-Z]|[0-9]|_)+");
	}
}
