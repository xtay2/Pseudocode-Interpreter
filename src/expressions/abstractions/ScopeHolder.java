package expressions.abstractions;

import expressions.abstractions.interfaces.MergedExpression;
import expressions.normal.brackets.OpenScope;
import modules.interpreter.Interpreter;
import types.AbstractType;

/**
 * Everything that has a {@link Scope}-Object in Storage.
 * 
 * <pre>
 * -This {@link Scope} should be saved with {@code private Scope scope = null;}
 * -Every {@link ScopeHolder} is a {@link MergedExpression}, 
 *  because the Scope has to be build in the Method {@link MergedExpression#merge()}.
 * -Every {@link ScopeHolder} should be a {@link MainExpression}.
 * </pre>
 * 
 */
public abstract class ScopeHolder extends MainExpression implements MergedExpression {

	/** Has to be private. Can be accessed via {@link ScopeHolder#getScope()}. */
	private Scope scope = null;

	/**
	 * Copies the following Constructor:
	 * 
	 * {@link Expression#Expression(int, Scope, AbstractType, AbstractType...))}.
	 */
	public ScopeHolder(int lineID, AbstractType myType, AbstractType... expected) {
		super(lineID, myType, expected);
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
	public final void initScope(OpenScope os) {
		if (os == null)
			throw new AssertionError("Open Scope cannot be null.");
		if (scope != null)
			throw new AssertionError("This Scope is already initialised with " + scope);
		scope = new Scope(type.toString(), os, os.getMatch(), os.getScope());
	}

	/**
	 * Initialise this function as native. (Without Body in the {@link GlobalScope})
	 */
	public final void initNative() {
		if (scope != null)
			throw new AssertionError("This Scope is already initialised with " + scope);
		scope = GlobalScope.GLOBAL;
	}

	/**
	 * Calls the first line in the scope.
	 */
	public final boolean callFirstLine() {
		return Interpreter.execute(scope.getStart() + 1);
	}

	/**
	 * Calls the next line after the closing scope bracket.
	 */
	@Override
	public final boolean callNextLine() {
		return Interpreter.execute(scope.getEnd());
	}
}
