package building.expressions.abstractions;

import building.expressions.possible.Call;
import building.expressions.possible.multicall.MultiCall;
import building.types.abstractions.SpecificType;

/**
 * Every class that extends this one has the possibility to stand alone in a line, (ie. be a main
 * expression), as well as being just a part of a line.
 *
 * If there's another definite MainExpression in the line, it gets choosen over this one.
 *
 * An {@link IllegalCodeFormatException} should be thrown when there are multiple
 * PossibleMainExpressions in a line.
 *
 * Some PossibleMainExpressions include: {@link Call} and {@link MultiCall}.
 *
 * @see Expression
 * @see MainExpression
 *
 */
public abstract class PossibleMainExpression extends MainExpression {

	/**
	 * Copies the following Constructor:
	 *
	 * {@link Expression#Expression(int, Scope, SpecificType)}.
	 */
	public PossibleMainExpression(int lineID, SpecificType myType) {
		super(lineID, myType);
	}
}
