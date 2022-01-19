package expressions.special;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.normal.Name;
import expressions.possible.Crement;
import parsing.program.ValueBuilder;

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

	/**
	 * Instead of implementing this, initialise this with
	 * {@link PossibleMainExpression#init} in the {@link ValueBuilder}.
	 * 
	 * @throws UnsupportedOperationException
	 */
	@Override
	public final void build(Expression... args) throws UnsupportedOperationException {
		throw new UnsupportedOperationException("Build shouldn't get implemented. Use init instead for " + this + ".");
	}

	/**
	 * This should be implemented instead of {@link MainExpression#build}, so it
	 * doesn't get called outside of the {@link ValueBuilder}.
	 * 
	 * @param name is an optional Name that the Expression might need.
	 * @param args are optional arguments needed for the Expression.
	 */
	public abstract void init(Name n, Expression... args);
}
