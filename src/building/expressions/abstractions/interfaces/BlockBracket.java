package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.Expression;
import building.expressions.main.CloseBlock;
import building.expressions.normal.brackets.OpenBlock;

/**
 * An interface for any Bracket, that has to be connected with the match.
 * 
 * @see OpenBlock
 * @see CloseBlock
 */
public interface BlockBracket {

	/** Returns the {@link Expression#lineIdentifier} of the matching {@link BlockBracket}. */
	public int getMatch();
}
