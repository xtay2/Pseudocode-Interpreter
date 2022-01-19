package expressions.main.functions;

import static helper.Output.print;
import static parsing.program.ExpressionType.NAME;

import java.util.Arrays;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.CastingException;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalReturnException;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.Keyword;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.normal.brackets.OpenBlock;
import expressions.special.DataType;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import extensions.datastructures.Dictionary;
import extensions.datastructures.DictionaryEntry;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;
import interpreter.system.SystemFunctions;
import parsing.finder.KeywordFinder;
import parsing.program.KeywordType;

public class Function extends Scope implements ValueHolder {

	// Keyword flags
	boolean isNative = false;
	protected Name name = null;
	private final Dictionary<Name, ExpectedType> paramBlueprint = new Dictionary<>();
	private DataType returnType = null;

	private Value returnVal = null;

	public Function(int line) {
		super(line);
		setExpectedExpressions(NAME);
	}

	@Override
	public void build(Expression... args) {
		int funcKeywordPos = 0;
		// Finde alle Keyword flags heraus.
		while (args[funcKeywordPos] instanceof Keyword k) {
			if (k.getKeyword() == KeywordType.NATIVE)
				isNative = true;
			funcKeywordPos++;
		}

		// Finde den Namen der Funktion heraus.
		if (args[funcKeywordPos + 1] instanceof Name n) {
			nameCheck(n.getName());
			name = n;
		} else
			throw new DeclarationException(getOriginalLine(), "Every function must have a name!" + Arrays.toString(args));

		// Finde die Namen und Typen der Parameter heraus.
		for (int i = funcKeywordPos + 2; i < args.length; i++) {
			if (args[i] instanceof ExpectedType) {
				if (args[i + 1] instanceof Name) {
					paramBlueprint.add((Name) args[i + 1], ((ExpectedType) args[i]));
					i += 2;
					continue;
				}
			} else if (args[i] instanceof Name) {
				paramBlueprint.add((Name) args[i], null);
				i++;
				continue;
			}
			// Finde heraus ob es einen Rückgabetypen gibt, und wenn ja, welchen.
			if (args[i] instanceof ExpectedReturnType) {
				if (!(args[i + 1] instanceof ExpectedType))
					throw new IllegalCodeFormatException(getOriginalLine(), "No type declaration after \"->\" in " + name);
				returnType = ((ExpectedType) args[i + 1]).type;
				break;
			}
		}
		Expression last = args[args.length - 1];

		if (!isNative) {
			if (last instanceof OpenBlock ob)
				block = ob;
			else
				throw new IllegalCodeFormatException(getOriginalLine(),
						name + ": A function-declaration must end with a valid block. Expected ':' or '{', was: '" + last + "'");
		}
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		if (isNative) {
			returnVal = SystemFunctions.callSystemFunc(SystemFunctions.getSystemFunction(getName()), params);
		} else {
			print("Executing " + name + (params.length == 0 ? "" : " with " + Arrays.toString(params)));
			registerParameters(params);
			if (doExecuteNext)
				Interpreter.execute(lineIdentifier + 1, true);
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

	public boolean isNative() {
		return isNative;
	}

	/**
	 * A function cannot be named after a keyword, a type, or the global-scope.
	 */
	private void nameCheck(String s) {
		if (KeywordFinder.isKeyword(s) || DataType.isType(s))
			throw new DeclarationException(getOriginalLine(), "A function cannot be named after a keyword or a type.");
	}

	/** Register all temporary function-vars */
	private void registerParameters(ValueHolder... params) {
		VarManager.registerScope(this);
		try {
			int paramCount = paramBlueprint.size();
			if (paramCount != params.length)
				throw new DeclarationException(getOriginalLine(),
						name + " takes " + paramCount + " parameters. Please call it accordingly.");
			for (int i = 0; i < paramCount; i++) {
				DictionaryEntry<Name, ExpectedType> param = paramBlueprint.get(i);
				Value v = params[i].getValue();
				Variable p = new Variable(lineIdentifier, param.getValue() == null ? v.getType() : param.getValue().type);
				p.initialise(param.getKey(), v);
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
		return Output.DEBUG ? this.getClass().getSimpleName() : (name == null ? "func" : name.toString());
	}

}
