package expressions.special;

import expressions.abstractions.Scope;
import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;

/**
 * An interface for any {@link Scope}-Bracket, that has to be connected with the
 * match.
 * 
 * @see OpenScope
 * @see CloseScope
 */
public interface Bracket {

	/** Find the matching Bracket */
	public Bracket getMatch();

	/** Set the matching Bracket. This should only get called once. */
	public void setMyMatch(Bracket match);
}
