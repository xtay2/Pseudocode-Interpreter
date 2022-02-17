package expressions.abstractions;

import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;
import interpreter.VarManager;

/**
 * A Scope limits the visibility of variables.
 * 
 * Every {@link ScopeHolder} contains a Scope.
 * 
 * @see OpenScope
 * @see CloseScope
 */
public class Scope {

	private final int openScope;
	private final int closeScope;

	private final String scopeName;

	/**
	 * Constructs a Scope and connects both Scope-Brackets.
	 * 
	 * @param scopeName is the name of this scope. This class adds the lineIDs to
	 *                  make it unique.
	 * @param os        is the {@link OpenScope} bracket. If null, the scope gets
	 *                  reduced to the lineID.
	 * @param cs        is the matching {@link CloseScope}.
	 */
	public Scope(String scopeName, OpenScope os, CloseScope cs) {
		this.openScope = os.lineIdentifier;
		this.closeScope = cs.lineIdentifier + 1;
		this.scopeName = scopeName + getStart() + "-" + getEnd();
		if (scopeName == null || os == null || cs == null)
			throw new AssertionError("Neither the name nor Open- or Close-Scope can be null.");
	}

	/**
	 * Protected Constructor for the {@link GlobalScope}.
	 */
	protected Scope() {
		this.scopeName = GlobalScope.NAME;
		this.openScope = 0;
		this.closeScope = Integer.MAX_VALUE;
	}

	/** Returns a universal identifying name for every scope. */
	public String getScopeName() {
		return scopeName;
	}

	/** Returns the start of this Scope. */
	public int getStart() {
		return openScope;
	}

	/** Returns the end of this Scope. */
	public int getEnd() {
		return closeScope;
	}

	/**
	 * Registers this scope at the {@link VarManager}.
	 * 
	 * Identical to {@code VarManager.registerScope(this);}
	 */
	public void reg() {
		VarManager.registerScope(this);
	}

	/**
	 * Deletes this scope at the {@link VarManager}.
	 * 
	 * Identical to {@code VarManager.deleteScope(this);}
	 */
	public void del() {
		VarManager.deleteScope(this);
	}
}