package expressions.main.functions;

import static helper.Output.print;

import java.util.Arrays;

import datatypes.Castable;
import exceptions.CastingException;
import exceptions.DeclarationException;
import expressions.main.CloseBlock;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.Name;
import expressions.normal.OpenBlock;
import expressions.normal.Variable;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.Type;
import expressions.special.ValueHolder;
import extensions.datastructures.Dictionary;
import extensions.datastructures.DictionaryEntry;
import interpreter.Interpreter;
import interpreter.VarManager;
import parser.finder.KeywordFinder;
import parser.program.ExpressionType;

public class Function extends MainExpression implements ValueHolder, Scope {

	private final Dictionary<Name, ExpectedType> paramBlueprint = new Dictionary<>();
	protected Name name = null;
	private Castable returnVal = null;
	private Type returnType = null;
	protected OpenBlock block = null;

	public Function(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		// Finde den Namen der Funktion heraus.
		if (!(args[1] instanceof Name))
			throw new DeclarationException("Every function must have a name!");
		nameCheck(((Name) args[1]).getName());
		name = ((Name) args[1]);
		// Finde die Namen und Typen der Parameter heraus.
		for (int i = 2; i < args.length; i++) {
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
					throw new IllegalArgumentException("No type declaration after \"->\" in " + name);
				returnType = ((ExpectedType) args[i + 1]).type;
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
		if (KeywordFinder.isKeyword(s) || Type.isType(s))
			throw new DeclarationException("A function cannot be named after a keyword or a type.");
	}

	/**
	 * This method gets called by the ReturnStatement. If a returntype is specified,
	 * the value gets implicitly casted.
	 */
	public void setReturnVal(Castable val) {
		if (returnVal != null && val != null)
			throw new IllegalStateException("Function " + name + " already has a return value.");
		if (returnType != null && val != null && val.getType() != returnType)
			returnVal = val.as(returnType);
		else
			returnVal = val;
	}

	@Override
	public Castable getValue() {
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
			if (paramCount != params.length)
				throw new DeclarationException(name + " takes " + paramCount + " parameters. Please call it accordingly.");
			for (int i = 0; i < paramCount; i++) {
				DictionaryEntry<Name, ExpectedType> param = paramBlueprint.get(i);
				Castable v = params[i].getValue();
				Variable p = new Variable(line, param.getValue() == null ? v.getType() : param.getValue().type);
				p.initialise(param.getKey(), v);
			}
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
		return "func" + name.getName() + getStart() + "-" + getEnd();
	}

	@Override
	public boolean isOneLineStatement() {
		return block == null;
	}

}
