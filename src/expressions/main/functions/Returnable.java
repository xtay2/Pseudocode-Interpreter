package expressions.main.functions;

import static types.specific.ExpressionType.NAME;
import static types.specific.KeywordType.FUNC;

import java.util.Set;

import datatypes.Value;
import exceptions.parsing.UnexpectedFlagException;
import expressions.abstractions.ScopeHolder;
import expressions.normal.containers.Name;
import expressions.normal.flag.Flaggable;
import helper.Output;
import modules.interpreter.Interpreter;
import types.specific.FlagType;
import types.specific.data.ExpectedType;

/**
 * This is the Superclass for {@link Function} and {@link NativeFunction}.
 * 
 * It provides the ability to set and give return-values.
 */
public abstract class Returnable extends ScopeHolder implements Flaggable {

	/** The {@link Name} of the underlying {@link Returnable}. */
	protected Name name = null;

	/** The expected return type. Null is equivalent to void. */
	protected ExpectedType returnType = null;

	/** This {@link Value} can be obtained after {@link #execute()}. */
	protected Value returnVal = null;

	/**
	 * Flags for this {@link Returnable}.
	 * 
	 * @deprecated WIP currently not used.
	 */
	@Deprecated
	protected Set<FlagType> flags = null;

	protected Returnable(int lineID) {
		super(lineID, FUNC, NAME);
	}

	/**
	 * This method gets called by the ReturnStatement. If a returntype is specified, the value gets
	 * implicitly casted.
	 */
	public final void setReturnVal(Value val) {
		if (returnVal != null && val != null)
			throw new AssertionError("Function " + name + " already has a return value.");
		if (returnType != null && val != null && val.type != returnType)
			returnVal = val.as(returnType);
		else
			returnVal = val;
	}

	/**
	 * Returns the return-value and resets it, so the function can get called again. This Method should
	 * only get called by {@link Interpreter#call}.
	 */
	public final Value retrieveReturnValue() {
		Value v = returnVal;
		returnVal = null;
		return v;
	}

	/** Returns the amount of expected parameters. */
	public abstract int expectedParams();

	/** Returns the {@link Name} of this {@link Function} as a {@link String}. */
	public final String getName() {
		return name.getName();
	}

	@Override
	public final void setFlags(Set<FlagType> flags) throws UnexpectedFlagException {
		this.flags = flags;
	}

	@Override
	public final String toString() {
		return Output.DEBUG ? getClass().getSimpleName() : name.getName();
	}
}
