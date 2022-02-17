package expressions.main.loops;

import static datatypes.numerical.NumberValue.ONE;
import static datatypes.numerical.NumberValue.ZERO;

import datatypes.numerical.DecimalValue;
import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import interpreter.VarManager;
import types.AbstractType;

/**
 * A loop is a Scope that gets called repeatedly, while or until a condition is true. This gets
 * testet by {@link Loop#doContinue()}.
 * 
 * @see ConditionalLoop
 * @see ForEachLoop
 * @see FromToLoop
 * @see RepeatLoop
 */
public abstract class Loop extends ScopeHolder {

	protected ValueHolder start = ZERO;
	protected ValueHolder inc = ONE;

	/**
	 * Copies the following Constructor:
	 * {@link Expression#Expression(int, AbstractType, AbstractType...)}.
	 */
	public Loop(int lineID, AbstractType myType, AbstractType... expected) {
		super(lineID, myType, expected);
	}

	/**
	 * Executes this loop as long as its run-condition is satisfied.
	 * 
	 * This method calls {@link Loop#doContinue()} for every iteration.
	 */
	@Override
	public final boolean execute(ValueHolder... params) {
		NumberValue i = start.getValue().asNumber();
		while (doContinue(i)) {
			getScope().reg();
			VarManager.initCounter(getScope(), i);
			if (!callFirstLine()) {
				getScope().del();
				return false;
			}
			getScope().del();
			i = i.add(inc.getValue().asNumber());
		}
		return callNextLine();
	}

	/**
	 * Only gets called by {@link Loop#execute()}.
	 * 
	 * @param iteration is the current iteration, starting at {@link DecimalValue#ZERO}.
	 */
	protected abstract boolean doContinue(NumberValue iteration);
}
