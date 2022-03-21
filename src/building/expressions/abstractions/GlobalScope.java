package building.expressions.abstractions;

import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.main.functions.MainFunction;
import building.types.specific.KeywordType;
import runtime.exceptions.DeclarationException;
import runtime.exceptions.DefNotFoundException;

/**
 * This is the basic {@link Scope} in which all other Scopes, Classes, Definitions and Variables may
 * get declared.
 */
public class GlobalScope extends Scope {

	public static final String NAME = "global";

	public static final GlobalScope GLOBAL = new GlobalScope();

	/** This should only get called once */
	private GlobalScope() {
		if (GLOBAL != null)
			throw new AssertionError("This constructor should only get called once by the constant.");
	}

	/**
	 * Returns the {@link MainFunction}, and removes it from the {@link Scope} afterwards. If it was
	 * defined in an inner {@link Scope}, a {@link DefNotFoundException} gets thrown.
	 */
	public MainFunction getMain() {
		Registerable r = get(KeywordType.MAIN.toString());
		if (r == null)
			throw new DefNotFoundException(-1,
					"Couldn't find main-function. \nIt has to be defined in the global-scope and can only get called once, by the interpreter.");
		memory.removeIf(e -> e.varName().equals(KeywordType.MAIN.toString()));
		if (r instanceof MainFunction m)
			return m;
		throw new DeclarationException(-1, "Nothing except the MainFunction can be called \"main\".");
	}
}
