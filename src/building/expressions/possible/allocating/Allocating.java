package building.expressions.possible.allocating;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Name;
import building.types.abstractions.SpecificType;

/**
 * The superclass for the {@link Assignment} and the {@link Declaration}.
 */
public abstract class Allocating extends PossibleMainExpression implements ValueHolder, NameHolder {

	/** Is the {@link Expression} which value should get changed. */
	protected final ValueChanger target;

	/** Is the {@link ValueHolder}, that contains the new Value for {@link #target}. */
	protected final ValueHolder val;

	public Allocating(int lineID, SpecificType myType, ValueChanger target, ValueHolder val) {
		super(lineID, myType);
		if (target == null || val == null)
			throw new AssertionError("Target or value cannot be null.");
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
