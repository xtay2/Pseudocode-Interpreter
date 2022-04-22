package building.expressions.main.functions;

import static building.types.specific.KeywordType.FUNC;
import static runtime.datatypes.MaybeValue.NULL;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.ReturnStatement;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.FlagType;
import building.types.specific.datatypes.DataType;
import interpreting.modules.merger.ExpressionMerger;
import runtime.datatypes.MaybeValue;
import runtime.datatypes.Value;
import runtime.exceptions.NullNotAllowedException;

/** This is the Superclass for {@link Function}, {@link MainFunction} and {@link NativeFunction}.
 * 
 * It provides the ability to set and give return-values. */
public abstract class Definition extends ScopeHolder implements Flaggable, NameHolder {

	/** The {@link Name} of the underlying {@link Definition}. */
	protected final Name name;

	/** The expected return type. Null is equivalent to void. */
	protected final DataType returnType;

	/** If true, {@link MaybeValue#NULL} can get returned from this {@link Definition}. */
	protected final boolean allowsNullAsReturn;

	/** This {@link Value} can be obtained after {@link #execute()}. */
	protected Value returnVal = null;

	/** Flags for this {@link Definition}. */
	protected final Set<FlagType> flags = new HashSet<>();

	/** Tells if this function already got called.
	 * 
	 * @see {@link FlagType#FINAL} */
	protected boolean wasCalled = false;

	/** Creates this {@link Definition}. It gets later registered in the {@link ExpressionMerger}.
	 * 
	 * @param name is the unique {@link Name} of this {@link Definition}.
	 * @param returnType can be null for {@link MainFunction} or if the {@link Definition} doesn't
	 * return anything.
	 * @param allowsNullAsReturn tells, if {@link MaybeValue#NULL} is a valid return-value.
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder} (Can be null for native funcs). */
	protected Definition(int lineID, Name name, DataType returnType, boolean allowsNullAsReturn, OpenBlock os) {
		super(lineID, FUNC, os);
		assert name != null : "Name cannot be null.";
		this.name = name;
		this.returnType = returnType;
		this.allowsNullAsReturn = allowsNullAsReturn;
	}

	/** This method gets called by the {@link ReturnStatement}. If a returntype is specified, the value
	 * gets implicitly casted. */
	public final void setValue(Value val) {
		if (returnVal != null && val != null)
			throw new AssertionError("Function \"" + name + "\" already has a return value.");
		if (val == NULL && !allowsNullAsReturn)
			throw new NullNotAllowedException(getOriginalLine(), "The " + type + " \"" + getNameString() + "\" tries to return null.");
		returnVal = val != NULL && val.type != returnType ? val.as(returnType) : val;

	}

	/** Returns the amount of expected parameters. */
	public abstract int expectedParams();

	@Override
	public final Name getName() {
		return name;
	}

	@Override
	public final void addFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}

	/** Calls this {@link Definition}
	 * 
	 * @param params optional call-parameters.
	 * @return an optional return-value. Default: null. */
	public abstract Value call(ValueHolder... params);

	@Override
	public final boolean execute() {
		throw new AssertionError("A func-declaration cannot be executed.");
	}

	@Override // This shouldn't get changed.
	public final String toString() {
		return getNameString() + "<" + expectedParams() + ">";
	}
}
