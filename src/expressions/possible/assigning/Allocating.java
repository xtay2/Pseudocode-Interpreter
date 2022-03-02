package expressions.possible.assigning;

import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.NameHolder;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Name;
import types.AbstractType;

/**
 * The superclass for the {@link Assignment} and the {@link Declaration}.
 */
public abstract class Allocating extends PossibleMainExpression implements ValueHolder, NameHolder {

	/** Is the {@link Expression} which value should get changed. */
	protected final ValueChanger target;

	/** Is the {@link ValueHolder}, that contains the new Value for {@link #target}. */
	protected final ValueHolder val;

	/**
	 * Copies the following Constructor:
	 * {@link Expression#Expression(int, AbstractType, AbstractType...)}.
	 */
	public Allocating(int lineID, AbstractType myType, ValueChanger target, ValueHolder val) {
		super(lineID, myType);
		if (target == null || val == null)
			throw new AssertionError("Target and Value cannot be null.");
		this.target = target;
		this.val = val;
	}

	/** Assigns the value and calls the next line afterwards. */
	@Override
	public final boolean execute() {
		getValue();
		return callNextLine();
	}

	@Override
	public final Name getName() {
		return target.getName();
	}
}
