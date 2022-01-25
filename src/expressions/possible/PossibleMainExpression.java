package expressions.possible;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.main.MainExpression;
import expressions.normal.Expression;
/**
 * Every class that extends this one has the possibility to stand alone in a
 * line, (ie be a main expression), as well as being just a part of a line.
 * 
 * If there's another definite MainExpression in the line, it gets choosen over
 * this one.
 * 
 * An {@link IllegalCodeFormatException} should be thrown when there are
 * multiple PossibleMainExpressions in a line.
 * 
 * Some PossibleMainExpressions include:
 * 
 * @see {@link Crement}
 * @see {@link Call}
 */
public abstract class PossibleMainExpression extends MainExpression {

	/**
	 * As defined in {@link Expression}.
	 */
	public PossibleMainExpression(int line) {
		super(line);
	}
}
