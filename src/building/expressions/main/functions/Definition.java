package building.expressions.main.functions;

import static building.types.specific.BuilderType.*;
import static building.types.specific.KeywordType.*;
import static runtime.datatypes.MaybeValue.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.main.statements.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import interpreting.modules.merger.*;
import misc.util.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

/**
 * This is the Superclass for {@link Function}, {@link MainFunction} and {@link NativeFunction}.
 *
 * It provides the ability to set and give return-values.
 */
public abstract class Definition extends BlockHolder implements Flaggable, NameHolder, Identifieable<Definition> {
	
	/** The {@link Name} of the underlying {@link Definition}. */
	protected final VarName name;
	
	/** The expected return type. Null is equivalent to void. */
	protected final DataType returnType;
	
	/** This {@link Value} can be obtained after {@link #execute()}. */
	protected Value returnVal = null;
	
	/** Flags for this {@link Definition}. */
	protected final Set<FlagType> flags = new HashSet<>();
	
	/**
	 * Tells if this function already got called.
	 *
	 * @see {@link FlagType#FINAL}
	 */
	protected boolean wasCalled = false;
	
	/**
	 * Creates this {@link Definition}. It gets later registered in the {@link ExpressionMerger}.
	 *
	 * @param name is the unique {@link Name} of this {@link Definition}.
	 * @param returnType can be null for {@link MainFunction} or if the {@link Definition} doesn't
	 * return anything.
	 * @param allowsNullAsReturn tells, if {@link MaybeValue#NULL} is a valid return-value.
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder} (Can be null for native funcs).
	 */
	protected Definition(int lineID, VarName name, DataType returnType, OpenBlock os) {
		super(lineID, FUNC, os);
		assert name != null : "Name cannot be null.";
		this.name = name;
		this.returnType = returnType;
	}
	
	/**
	 * This method gets called by the {@link ReturnStatement}. It checks, if the {@link Value} passes
	 * the {@link #returnType}.
	 */
	public final void setValue(Value val) {
		if (returnVal != null && val != null)
			throw new AssertionError("Function \"" + name + "\" already has a return value.");
		if (returnType == null) { // No return-type specified.
			String suggDataType = (val instanceof ArrayValue av ? av.getRules() : val.dataType).toString();
			throw new PseudocodeException("VoidReturn", //
					"Doesn't specify " + suggDataType + " as return-type, but returned a value anyways." + //
							"\nTo add a return-type, write: " + //
							FUNC + " " + getNameString() + "(...) " + ARROW_R + " " + suggDataType + " " + OPEN_BLOCK,
					getBlueprintPath());
		}
		if (val == NULL && !returnType.allowsNull) {
			throw new PseudocodeException("NullNotAllowed", //
					"The " + type + " \"" + getNameString() + "\" tries to return null.", getBlueprintPath());
		}
		if (!val.matches(returnType)) {
			throw new PseudocodeException("UnexpectedReturn", //
					"The function " + getNameString() + " expected a return-value of type " + returnType //
							+ " but returned the value " + val + "instead.",
					getBlueprintPath());
		}
		returnVal = val;
	}
	
	/** Returns the amount of expected parameters. */
	public abstract int expectedParams();
	
	@Override
	public final Name getName() { return name; }
	
	@Override
	public final void addFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}
	
	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}
	
	/**
	 * Calls this {@link Definition}
	 *
	 * @param params optional call-parameters.
	 * @return an optional return-value. Default: null.
	 */
	public abstract Value call(ValueHolder... params);
	
	@Override
	public final ID<Definition> generateID() {
		return new ID<>(getNameString(), expectedParams());
	}
	
	@Override
	public final boolean execute() {
		throw new AssertionError("A func-declaration cannot be executed.");
	}
	
	@Override // This shouldn't get changed as its used in the error messages of Scope
	public final String toString() {
		return getNameString() + "<" + expectedParams() + ">";
	}
}
