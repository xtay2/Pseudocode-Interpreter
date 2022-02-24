package expressions.possible.assigning;

import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import types.AbstractType;

/**
 * The superclass for the {@link Assignment} and the {@link Declaration}.
 */
public abstract class Allocating extends PossibleMainExpression implements ValueHolder, MergedExpression {

	/** Is the {@link Expression} which value should get changed. */
	protected ValueChanger target;

	/** Is the {@link ValueHolder}, that contains the new Value for {@link #target}. */
	protected ValueHolder val;

	/**
	 * Copies the following Constructor:
	 * {@link Expression#Expression(int, AbstractType, AbstractType...)}.
	 */
	public Allocating(int lineID, AbstractType myType, AbstractType... expected) {
		super(lineID, myType, expected);
	}

	/** Assigns the value and calls the next line afterwards. */
	@Override
	public final boolean execute(ValueHolder... params) {
		getValue();
		return callNextLine();
	}
}
