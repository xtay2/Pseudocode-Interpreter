package expressions.special;

import expressions.main.CloseScope;
import expressions.normal.brackets.CloseBracket;
import expressions.normal.brackets.OpenBracket;
import expressions.normal.brackets.OpenScope;

/**
 * {@link OpenBracket} {@link CloseBracket}
 * 
 * {@link OpenScope} {@link CloseScope}
 */
public interface Bracket {

	/** Find the matching Bracket */
	public Bracket getMatch();

	/** Set the matching Bracket. This should only get called once. */
	public void setMyMatch(Bracket match);
}
