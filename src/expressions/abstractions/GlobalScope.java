package expressions.abstractions;

/**
 * This is the basic scope in which all other Scopes, Classes, Functions and Variables may get
 * declared.
 */
public class GlobalScope extends Scope {

	public static final String NAME = "global";

	public static final GlobalScope GLOBAL = new GlobalScope();

	/** This should only get called once */
	private GlobalScope() {
		if (GLOBAL != null)
			throw new AssertionError("This constructor should only get called once by the constant.");
	}
}
