package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.*;
import building.expressions.main.*;
import building.expressions.normal.brackets.*;

/**
 * An interface for any Bracket, that has to be connected with the match.
 *
 * @see OpenBlock
 * @see CloseBlock
 */
public interface BlockBracket extends AbstractExpression {
	
	/** Returns the {@link Expression#lineIdentifier} of the matching {@link BlockBracket}. */
	int getMatch();
}
