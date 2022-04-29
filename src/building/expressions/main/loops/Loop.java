package building.expressions.main.loops;

import static runtime.datatypes.numerical.NumberValue.ONE;
import static runtime.datatypes.numerical.NumberValue.ZERO;

import java.util.Set;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.types.abstractions.SpecificType;
import building.types.specific.FlagType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.numerical.DecimalValue;
import runtime.datatypes.numerical.NumberValue;

/**
 * A loop is a Scope that gets called repeatedly, while or until a condition is true. This gets
 * testet by {@link Loop#doContinue()}.
 * 
 * @see ConditionalLoop
 * @see ForEachLoop
 * @see IntervalLoop
 * @see RepeatLoop
 */
public abstract class Loop extends ScopeHolder {

	// These should get initialised at merge
	protected ValueHolder startHolder = ZERO;
	protected ValueHolder incHolder = ONE;
	protected final Name alias;

	// These should get initialised at init
	protected NumberValue start;
	protected NumberValue inc;

	/**
	 * Constructor for an abstract {@link Loop}.
	 * 
	 * @param lineID
	 * @param myType
	 * @param alias is an optional name for a loopcounter. (Can be null)
	 * @param os
	 */
	public Loop(int lineID, SpecificType myType, Name alias, OpenBlock os) {
		super(lineID, myType, os);
		this.alias = alias;
	}

	/**
	 * Executes this loop as long as its run-condition is satisfied.
	 * 
	 * This method calls {@link Loop#doContinue()} for every iteration.
	 */
	@Override
	public final boolean execute() {
		initLoop();
		if (start == null || inc == null)
			throw new AssertionError("Start and end have to be initialised. See: initLoop()");
		NumberValue i = start;
		Name cntName = getLoopVarAlias();
		while (doContinue(i)) {
			initCounter(i, cntName);
			if (!callFirstLine()) {
				getScope().clear();
				return false;
			}
			getScope().clear();
			i = i.add(inc);
		}
		return callNextLine();
	}

	/**
	 * Sets the immutable counter for this iteration.
	 * 
	 * @param i is the {@link NumberValue} of the counter
	 * @param cntName is the pre-defined countername "i"-"p".
	 */
	private void initCounter(NumberValue i, Name cntName) {
		new Variable(lineIdentifier, getScope(), SingleType.NUMBER, false, cntName, i).addFlags(Set.of(FlagType.CONSTANT));
	}

	/**
	 * Only gets called by {@link Loop#execute()} before a loop gets executed.
	 * 
	 * Set {@link #start} and {@link #inc} here.
	 */
	protected void initLoop() {
		start = startHolder.getValue().asNumber();
		inc = incHolder.getValue().asNumber();
	}

	/**
	 * Only gets called by {@link Loop#execute()}.
	 * 
	 * @param iteration is the current iteration, starting at {@link DecimalValue#ZERO}.
	 */
	protected abstract boolean doContinue(NumberValue iteration);

	protected Name getLoopVarAlias() {
		return alias != null ? alias : getScope().getCounterName(getOriginalLine());
	}
}
