package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.main.CloseScope;
import building.expressions.normal.brackets.OpenScope;

/**
 * An interface for any {@link Scope}-Bracket, that has to be connected with the match.
 * 
 * @see OpenScope
 * @see CloseScope
 */
public interface ScopeBracket {

	/** Returns the {@link Expression#lineIdentifier} of the matching {@link ScopeBracket}. */
	public int getMatch();
}
