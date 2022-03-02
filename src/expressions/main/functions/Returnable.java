package expressions.main.functions;

import static types.specific.FlagType.FINAL;
import static types.specific.KeywordType.FUNC;

import java.util.HashSet;
import java.util.Set;

import datatypes.Value;
import exceptions.parsing.UnexpectedFlagException;
import exceptions.runtime.IllegalCallException;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.Callable;
import expressions.abstractions.interfaces.NameHolder;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.flag.Flaggable;
import helper.Output;
import modules.interpreter.FuncManager;
import types.specific.FlagType;
import types.specific.data.ExpectedType;

/**
 * This is the Superclass for {@link Function} and {@link NativeFunction}.
 * 
 * It provides the ability to set and give return-values.
 */
public abstract class Returnable extends ScopeHolder implements Flaggable, Callable, NameHolder {

	/** The {@link Name} of the underlying {@link Returnable}. */
	protected final Name name;

	/** The expected return type. Null is equivalent to void. */
	protected ExpectedType returnType = null;

	/** This {@link Value} can be obtained after {@link #execute()}. */
	protected Value returnVal = null;

	/**
	 * Flags for this {@link Returnable}.
	 */
	protected Set<FlagType> flags = new HashSet<>();

	/**
	 * Tells if this function already got called.
	 * 
	 * @see {@link FlagType#FINAL}
	 */
	protected boolean wasCalled = false;

	protected Returnable(int lineID, Name name, OpenScope os) {
		super(lineID, FUNC, os);
		if (name == null)
			throw new AssertionError("Name cannot be null.");
		FuncManager.registerFunction(name.getNameString(), lineID);
		this.name = name;
	}

	/**
	 * This method gets called by the ReturnStatement. If a returntype is specified, the value gets
	 * implicitly casted.
	 */
	public final void setValue(Value val) {
		if (returnVal != null && val != null)
			throw new AssertionError("Function " + name + " already has a return value.");
		if (returnType != null && val != null && val.type != returnType)
			returnVal = val.as(returnType);
		else
			returnVal = val;
	}

	/** Returns the amount of expected parameters. */
	public abstract int expectedParams();

	@Override
	public final Name getName() {
		return name;
	}

	@Override
	public final void setFlags(Set<FlagType> flags) throws UnexpectedFlagException {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}

	/**
	 * Checks, if this {@link Returnable} is {@link FlagType#FINAL} and already got called.
	 * 
	 * If both is true, an {@link IllegalCallException} gets thrown.
	 * 
	 * This method gets called by all {@link #execute()}-Methods.
	 */
	protected final void finalCheck() {
		if (hasFlag(FINAL)) {
			if (wasCalled)
				throw new IllegalCallException(getOriginalLine(),
						"Function \"" + getNameString() + "\" is declared as final and can only get called once.");
			wasCalled = true;
		}
	}

	@Override
	public final boolean execute() {
		throw new AssertionError("A func-declaration cannot be executed.");
	}

	@Override
	public final String toString() {
		return Output.DEBUG ? getClass().getSimpleName() : getNameString();
	}
}
