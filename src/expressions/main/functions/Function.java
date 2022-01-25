package expressions.main.functions;

import static helper.Output.print;
import static parsing.program.ExpressionType.NAME;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.parsing.UnexpectedFlagException;
import exceptions.runtime.CastingException;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalReturnException;
import expressions.normal.ExpectedType;
import expressions.normal.Expression;
import expressions.normal.Flag;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.normal.brackets.OpenScope;
import expressions.special.DataType;
import expressions.special.Flaggable;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;
import interpreter.system.SystemFunctions;

public class Function extends Scope implements ValueHolder, Flaggable {

	// Keyword flags
	boolean isNative = false;
	protected Name name = null;
	private HashMap<Name, ExpectedType> paramBlueprint;
	private DataType returnType = null;

	private Value returnVal = null;

	public Function(int line) {
		super(line);
		setExpectedExpressions(NAME);
	}

	/** [NAME] (?[?TYPE] [NAME]) [EXPECTED_RETURN_TYPE] [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		paramBlueprint = new HashMap<>();
		name = (Name) e[0];
		// Finde die Namen und Typen der Parameter heraus.
		for (int i = 1; i < e.length - 2; i++) {
			if (e[i] instanceof ExpectedType) {
				if (e[i + 1] instanceof Name) {
					paramBlueprint.put((Name) e[i + 1], ((ExpectedType) e[i]));
					i += 2;
					continue;
				}
				throw new IllegalCodeFormatException(getOriginalLine(), "Expected a name after the type " + e[i] + ". Got " + e[i + 1] + " instead.");
			} else if (e[i] instanceof Name) {
				paramBlueprint.put((Name) e[i], null);
				i++;
				continue;
			}
			throw new AssertionError("Unexpected token: " + e[i]);
		}
		if (e[e.length - 2] instanceof ExpectedType t)
			returnType = t.type;
		else
			returnType = DataType.VAR;
		if (!isNative)
			block = (OpenScope) e[e.length - 1];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		if (isNative) {
			returnVal = SystemFunctions.callSystemFunc(SystemFunctions.getSystemFunction(getName()), params);
		} else {
			print("Executing " + name + (params.length == 0 ? "" : " with " + Arrays.toString(params)));
			registerParameters(params);
			if (returnType != null && returnVal == null)
				throw new IllegalReturnException(getOriginalLine(),
						"func " + name + " was defined to return a value of type: " + returnType.getName() + ", but returned nothing.");
			VarManager.deleteScope(this);
		}
		return true;
	}

	/** Returns the amount of expected parameters. */
	public int expectedParams() {
		return paramBlueprint.size();
	}

	@Override
	public int getEnd() {
		return isNative ? getStart() : super.getEnd();
	}

	public String getName() {
		return name.getName();
	}

	@Override
	public String getScopeName() {
		return "func" + name.getName() + getStart() + "-" + getEnd();
	}

	@Override
	public Value getValue() {
		return returnVal;
	}

	@Override
	public void setFlags(List<Flag> flags) throws UnexpectedFlagException {
		while (!flags.isEmpty()) {
			switch (((Flag) flags.remove(0)).flagType) {
			case NATIVE -> isNative = true;
			default -> throw new UnexpectedFlagException(getOriginalLine(), " Flagtype has to be defined!");
			}
		}
	}

	@Override
	public boolean isNative() {
		return isNative;
	}

	/** Register all temporary function-vars */
	private void registerParameters(ValueHolder... params) {
		VarManager.registerScope(this);
		try {
			int paramCount = paramBlueprint.size();
			if (paramCount != params.length)
				throw new DeclarationException(getOriginalLine(),
						name + " takes " + paramCount + " parameters. Please call it accordingly.");
			int i = 0;
			for (Entry<Name, ExpectedType> param : paramBlueprint.entrySet()) {
				Value v = params[i++].getValue();
				VarManager.registerVar(
						new Variable(lineIdentifier, param.getValue() == null ? v.getType() : param.getValue().type, param.getKey(), v));
			}
		} catch (CastingException e) {
			throw new DeclarationException(getOriginalLine(), "Passed a value with an unwanted type to " + name + ".");
		}
	}

	/**
	 * This method gets called by the ReturnStatement. If a returntype is specified,
	 * the value gets implicitly casted.
	 */
	public void setReturnVal(Value val) {
		if (returnVal != null && val != null)
			throw new AssertionError("Function " + name + " already has a return value.");
		if (returnType != null && val != null && val.getType() != returnType)
			returnVal = val.as(returnType);
		else
			returnVal = val;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? (isNative ? "Native " : "") + getClass().getSimpleName() : name.getName();
	}
}
