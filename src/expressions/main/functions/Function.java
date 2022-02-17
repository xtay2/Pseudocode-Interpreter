package expressions.main.functions;

import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.parsing.UnexpectedFlagException;
import exceptions.runtime.IllegalCallException;
import exceptions.runtime.IllegalReturnException;
import expressions.abstractions.Expression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.ExpectedType;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import expressions.normal.flag.Flaggable;
import expressions.possible.Call;
import helper.Output;
import interpreter.Interpreter;
import interpreter.system.SystemFunctions;
import parsing.program.ProgramLine;
import types.ExpressionType;
import types.specific.DataType;
import types.specific.FlagType;
import types.specific.KeywordType;

/**
 * This is the class for a Function-Declaration.
 * 
 * If a Function gets called, this happens through the {@link Call}-Class and
 * {@link Interpreter#call}.
 */
public class Function extends ScopeHolder implements Flaggable {

	// Keyword flags
	boolean isNative = false;

	/** The {@link Name} of this {@link Function}. */
	protected Name name = null;

	/** All expected parameters. */
	private HashMap<Name, ExpectedType> paramBlueprint = new HashMap<>();

	/** The expected return type. Null is equivalent to void. */
	private DataType returnType = null;

	/** This {@link Value} can be obtained after {@link #execute()}. */
	private Value returnVal = null;

	public Function(int lineID) {
		super(lineID, KeywordType.FUNC, ExpressionType.NAME);
	}

	/** [NAME] (?[?TYPE] [NAME]) ([EXPECTED_RETURN_TYPE]) [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		name = (Name) e[0];
		// Names and Types of parameters.
		for (int i = 1; i < e.length - 2; i++) {
			if (e[i] instanceof ExpectedType) {
				if (e[i + 1] instanceof Name) {
					paramBlueprint.put((Name) e[i + 1], ((ExpectedType) e[i]));
					i += 2;
					continue;
				}
				throw new IllegalCodeFormatException(getOriginalLine(),
						"Expected a name after the type " + e[i] + ". Got " + e[i + 1] + " instead.");
			}
			if (e[i] instanceof Name) {
				paramBlueprint.put((Name) e[i], null);
				i++;
				continue;
			}
			throw new AssertionError("Unexpected token: " + e[i]);
		}
		// Optional Expected ReturnValue
		if (e[e.length - 2] instanceof ExpectedType t)
			returnType = t.type;
		initScope((OpenScope) e[e.length - 1]);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		if (paramBlueprint.size() != params.length)
			throw new IllegalCallException(getOriginalLine(), "This function is called with the wrong amount of params.");
		if (isNative())
			returnVal = SystemFunctions.callSystemFunc(SystemFunctions.getSystemFunction(getName()), params);
		else {
			getScope().reg();
			// Init Params
			int i = 0;
			for (Entry<Name, ExpectedType> param : paramBlueprint.entrySet()) {
				Value v = params[i++].getValue();
				Variable.quickCreate(lineIdentifier, (DataType) v.type, param.getKey(), v);
			}
			callFirstLine();
			// The return-value is now set.
			if (returnType != null && returnVal == null) {
				throw new IllegalReturnException(getOriginalLine(),
						getName() + " was defined to return a value of type: " + returnType + ", but returned nothing.");
			}
			getScope().del();
		}
		return true;
	}

	/** Returns the amount of expected parameters. */
	public int expectedParams() {
		return paramBlueprint.size();
	}

	/** Returns the {@link Name} of this {@link Function} as a {@link String}. */
	public String getName() {
		return name.getName();
	}

	/**
	 * This method gets called by the ReturnStatement. If a returntype is specified, the value gets
	 * implicitly casted.
	 */
	public void setReturnVal(Value val) {
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
	public Value retrieveReturnValue() {
		Value v = returnVal;
		returnVal = null;
		return v;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? (isNative() ? "Native " : "") + getClass().getSimpleName() : name.getName();
	}

	/**
	 * Sets the flags for this {@link Function}. Viable flags include:
	 * 
	 * <pre>
	 * - {@link FlagType#NATIVE} tells, that this Function is defined in {@link SystemFunctions}.
	 * </pre>
	 */
	@Override
	public void setFlags(List<FlagType> flags) throws UnexpectedFlagException {
		for (FlagType f : flags) {
			switch (f) {
			case NATIVE -> isNative = true;
			default -> throw new UnexpectedFlagException(getOriginalLine(), f + " isnt a valid flag for a function.");
			}
		}
	}

	/** Gets called in {@link ProgramLine#searchForScope()}. */
	public boolean isNative() {
		return isNative;
	}
}
