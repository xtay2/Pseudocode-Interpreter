package expressions.abstractions;

import modules.interpreter.VarManager;

/**
 * This is the basic scope in which all other Scopes, Classes, Functions and
 * Variables may get declared.
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
	 * Registers this scope at the {@link VarManager}.
	 * 
	 * Identical to {@code VarManager.registerScope(this);}
	 */
	public void reg() {
		throw new UnsupportedOperationException("The " + NAME + "-scope is always registered.");
	}

	/**
	 * Deletes this scope at the {@link VarManager}.
	 * 
	 * Identical to {@code VarManager.deleteScope(this);}
	 */
	public void del() {
		throw new UnsupportedOperationException("The " + NAME + "-scope cannot be deleted.");
	}

}
