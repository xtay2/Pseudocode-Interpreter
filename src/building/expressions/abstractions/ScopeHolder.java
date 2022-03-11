package building.expressions.abstractions;

import building.expressions.main.functions.Definition;
import building.expressions.main.loops.Loop;
import building.expressions.normal.brackets.OpenBlock;
import building.types.AbstractType;
import interpreting.program.Program;

/**
 * Everything that has a {@link Scope}-Object in Storage.
 * 
 * <pre>
 * -This {@link Scope} should be saved with {@code private Scope scope = null;}
 * -Every {@link ScopeHolder} should be a {@link MainExpression}.
 * </pre>
 * 
 * @see Definition
 * @see Loop
 * 
 */
public abstract class ScopeHolder extends BlockHolder {

	/** Has to be private. Can be accessed via {@link ScopeHolder#getScope()}. */
	private Scope scope = null;

	/**
	 * Creates a {@link ScopeHolder}.
	 * 
	 * @param myType shouldn't be null.
	 * @param ob     can be null for native scopes
	 */
	public ScopeHolder(int lineID, AbstractType myType, OpenBlock ob) {
		super(lineID, myType, ob);
	}

	/** Returns the Scope of this {@link ScopeHolder}. */
	@Override
	public final Scope getScope() {
		if (scope == null)
			throw new AssertionError("The scope has to be initialised.");
		return scope;
	}

	/** Returns the Scope this {@link ScopeHolder} lies in. */
	public final Scope getOuterScope() {
		return super.getScope();
	}

	/**
	 * Initialises the Scope with a unique name.
	 * 
	 * @param os has to know its match at this point.
	 */
	private final void initScope(OpenBlock ob) {
		if (ob == null)
			throw new AssertionError("Open Scope cannot be null.");
		if (scope != null)
			throw new AssertionError("This Scope is already initialised with " + scope);
		scope = new Scope(ob.lineIdentifier, type.toString(), getOuterScope());
	}

	/**
	 * Initialise this function as native. (Without Body in the {@link GlobalScope})
	 */
	private final void initNative() {
		if (scope != null)
			throw new AssertionError("This Scope is already initialised with " + scope);
		scope = GlobalScope.GLOBAL;
	}

	/**
	 * Gets called by {@link Program#constructAndMerge()}.
	 * 
	 * Calls {@link #initNative()} or {@link #initScope(OpenBlock)}.
	 */
	public final void initScope() {
		if (ob == null)
			initNative();
		else
			initScope(ob);
	}
}
