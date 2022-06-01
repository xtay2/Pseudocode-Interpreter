package building.expressions.abstractions;

import building.expressions.main.functions.MainFunction;
import building.types.specific.KeywordType;

/**
 * This is the basic {@link Scope} in which all other Scopes, Classes, Definitions and Variables may
 * get declared.
 */
public class GlobalScope extends Scope {

	public static final String NAME = "global";

	public static final GlobalScope GLOBAL = new GlobalScope();

	/** This should only get called once */
	private GlobalScope() {
		assert GLOBAL == null : "This constructor should only get called once by the constant.";
	}

	/**
	 * Returns the {@link MainFunction}, and removes it from the {@link Scope} afterwards. If it was
	 * defined in an inner {@link Scope}, a {@link DefNotFoundException} gets thrown.
	 */
	public MainFunction getMain() {
		MainFunction r = (MainFunction) get(KeywordType.MAIN.toString());
		assert r != null : "Couldn't find main-function.";
		memory.removeIf(e -> e.varName().equals(KeywordType.MAIN.toString()));
		return r;
	}
}
