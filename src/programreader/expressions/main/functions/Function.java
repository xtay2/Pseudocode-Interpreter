package programreader.expressions.main.functions;

import java.util.Arrays;

import exceptions.CastingException;
import exceptions.DeclarationException;
import extensions.datastructures.dict.Dictionary;
import extensions.datastructures.dict.DictionaryEntry;
import programreader.expressions.main.CloseBlock;
import programreader.expressions.normal.ExpectedReturnType;
import programreader.expressions.normal.ExpectedType;
import programreader.expressions.normal.Name;
import programreader.expressions.normal.OpenBlock;
import programreader.expressions.normal.TypedVar;
import programreader.expressions.normal.Variable;
import programreader.expressions.special.Expression;
import programreader.expressions.special.MainExpression;
import programreader.expressions.special.Scope;
import programreader.expressions.special.Type;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;
import programreader.finder.KeywordFinder;
import programreader.interpreter.Interpreter;
import programreader.interpreter.VarManager;
import programreader.program.ExpressionType;
import static helper.Output.*;

public class Function extends MainExpression implements ValueHolder, Scope {

	private final Dictionary<Name, ExpectedType> paramBlueprint = new Dictionary<>();
	protected Name name = null;
	private Value returnVal = null;
	private Type returnType = null;
	protected OpenBlock block = null;

	public Function(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		// Finde den Namen der Funktion heraus.
		if (args[1] instanceof Name) {
			nameCheck(((Name) args[1]).getName());
			name = ((Name) args[1]);
		} else
			throw new DeclarationException("Every function must have a name!");
		// Finde die Namen und Typen der Parameter heraus.
		for (int i = 2; i < args.length; i++) {
			if (args[i] instanceof ExpectedType) {
				if (args[i + 1] instanceof Name) {
					paramBlueprint.add((Name) args[i + 1], ((ExpectedType) args[i]));
					i += 2;
					continue;
				}
				paramBlueprint.add((Name) args[i + 1], null);
				i++;
				continue;
			}
			// Finde heraus ob es einen Rückgabetypen gibt, und wenn ja, welchen.
			if (args[i] instanceof ExpectedReturnType) {
				if (args[i + 1] instanceof ExpectedType)
					returnType = ((ExpectedType) args[i + 1]).type;
				else
					throw new IllegalArgumentException("No type declaration after \"->\" in " + name);
				break;
			}
		}
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	/**
	 * A function cannot be named after a keyword, a type, or the global-scope.
	 */
	private void nameCheck(String s) {
		if (KeywordFinder.isKeyword(s) || Type.isType(s) || GLOBAL_SCOPE.getScopeName().equals(s))
			throw new DeclarationException("A function cannot be named after a keyword, a type, or the global-scope.");
	}

	/**
	 * This method gets called by the ReturnStatement. If a returntype is specified,
	 * the value gets implicitly casted.
	 */
	public void setReturnVal(Value val) {
		if (returnVal != null && val != null)
			throw new IllegalStateException("Function " + name + " already has a return value.");
		if (returnType != null && val != null && val.getType() != returnType) {
			if (returnType == Type.BOOL)
				returnVal = new Value(val.asBool(), Type.BOOL);
			else if (returnType == Type.TEXT)
				returnVal = new Value(val.asText(), Type.TEXT);
			else if (returnType == Type.NUMBER) {
				Number nr = val.asNr();
				if (nr instanceof Integer)
					returnVal = new Value((Integer) nr, Type.NUMBER);
				else
					returnVal = new Value((Integer) nr, Type.NUMBER);
			}
		} else
			returnVal = val;
	}

	@Override
	public Value getValue() {
		return returnVal;
	}

	/** Returns the amount of expected parameters. */
	public int expectedParams() {
		return paramBlueprint.size();
	}

	public String getName() {
		return name.getName();
	}

	/** Register all temporary function-vars */
	private void registerParameters(ValueHolder... params) {
		VarManager.registerScope(this);
		try {
			int paramCount = paramBlueprint.size();
			if (paramCount == params.length) {
				for (int i = 0; i < paramCount; i++) {
					DictionaryEntry<Name, ExpectedType> param = paramBlueprint.get(i);
					Variable p = param.getValue() == null ? new Variable(line) : new TypedVar(param.getValue().type, line);
					p.initialise(param.getKey(), params[i].getValue());
				}
			} else
				throw new DeclarationException(name + " takes " + paramCount + " parameters. Please call it accordingly.");
		} catch (CastingException e) {
			throw new DeclarationException("Passed a value with an unwanted type to " + name + ".");
		}
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing " + name + (params.length == 0 ? "" : " with " + Arrays.toString(params)));
		registerParameters(params);
		if (doExecuteNext)
			Interpreter.execute(line + 1, !isOneLineStatement());
		if (returnType != null && returnVal == null)
			throw new IllegalArgumentException(
					"func " + name + " was defined to return a value of type: " + returnType.getName() + ", but returned nothing.");
		VarManager.deleteScope(this);
		return true;
	}

	@Override
	public int getStart() {
		return line;
	}

	@Override
	public int getEnd() {
		return isOneLineStatement() || block.getMatch() == null ? line + 2 : ((CloseBlock) block.getMatch()).line + 1;
	}

	@Override
	public String getScopeName() {
		return name.getName() + getStart() + "-" + getEnd();
	}

	@Override
	public boolean isOneLineStatement() {
		return block == null;
	}

}
