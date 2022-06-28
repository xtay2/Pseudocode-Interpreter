package building.expressions.main.loops;

import static runtime.datatypes.numerical.NumberValue.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.types.abstractions.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import runtime.datatypes.numerical.*;

/**
 * A loop is a Scope that gets called repeatedly, while or until a condition is true. This gets
 * testet by {@link Loop#doContinue()}.
 *
 * @see ConditionalLoop
 * @see ForEachLoop
 * @see IntervalLoop
 * @see RepeatLoop
 */
public abstract class Loop extends BlockHolder implements ScopeHolder {
	
	// These should get initialised at merge
	protected ValueHolder startHolder = ZERO;
	protected ValueHolder incHolder = ONE;
	private final Name alias;
	
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
			if (!callFirstLine())
				return false;
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
		new Variable(lineIdentifier, new DataType(SingleType.NR, false), cntName, i).addFlags(Set.of(FlagType.CONSTANT));
	}
	
	/**
	 * Only gets called by {@link Loop#execute()} before a loop gets executed.
	 *
	 * Set {@link #start} and {@link #inc} here.
	 */
	protected void initLoop() {
		try {
			start = startHolder.asNr();
			inc = incHolder.asNr();
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
	}
	
	/**
	 * Only gets called by {@link Loop#execute()}.
	 *
	 * @param iteration is the current iteration, starting at {@link DecimalValue#ZERO}.
	 */
	protected abstract boolean doContinue(NumberValue iteration);
	
	/**
	 * Returns the name for the loop-counter.
	 *
	 * @return {@link #alias} if existing, or the implicit countername, if not.
	 */
	protected final Name getLoopVarAlias() { return alias != null ? alias : ScopeManager.getCounterName(getBlueprintPath().blueprint); }
}
