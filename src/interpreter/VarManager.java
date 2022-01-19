package interpreter;

import static helper.Output.print;

import java.util.ArrayList;
import java.util.HashMap;

import datatypes.NumberValue;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalCallException;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.special.DataType;
import expressions.special.Scope;
import parsing.finder.KeywordFinder;

class ScopeMemory {

	private final HashMap<String, Variable> scope = new HashMap<>();

	private final String scopeName;

	public ScopeMemory(String scopeName) {
		this.scopeName = scopeName;
	}

	public String getName() {
		return scopeName;
	}

	public HashMap<String, Variable> getScope() {
		return scope;
	}

	@Override
	public String toString() {
		return "[Scope " + scopeName + ": " + scope + "]";
	}

}

final class Stack {

	private final ArrayList<ScopeMemory> scopes = new ArrayList<>();

	public void appendScope(String scopeName) {
		scopes.add(new ScopeMemory(scopeName));
	}

	public Variable findVar(String varName, int calledInLine) {
		for (int i = scopes.size() - 1; i >= 0; i--) {
			HashMap<String, Variable> scope = scopes.get(i).getScope();
			Variable var = scope.get(varName);
			if (var != null)
				return var;
		}
		throw new IllegalCallException(calledInLine, "Called var " + varName + " doesn't exist. \nScopes: " + scopes);
	}

	public int height() {
		return scopes.size();
	}

	private HashMap<String, Variable> peekScope() {
		return scopes.get(height() - 1).getScope();
	}

	public String peekScopeName() {
		return scopes.get(height() - 1).getName();
	}

	public ScopeMemory popScope(String name) {
		if (!name.equals(scopes.get(height() - 1).getName()))
			throw new IllegalArgumentException(
					"Trying to delete non-top-scope " + name + "\ntop-scope was " + scopes.get(height() - 1).getName() + "\n" + this);
		return scopes.remove(height() - 1);
	}

	public void registerVar(Variable var) {
		if (peekScope().containsValue(var))
			throw new IllegalArgumentException("Var \"" + var.getName() + "\" couldn't be registered in scope \""
					+ scopes.get(height() - 1).getScope() + "\", as it exists here already.");
		peekScope().put(var.getName(), var);
	}

	@Override
	public String toString() {
		return scopes.toString();
	}
}

public final class VarManager {

	private static final char FIRST_COUNTER_NAME = 'i';
	private static char counterName = FIRST_COUNTER_NAME;
	private static final byte LOOP_VAR_COUNT = 7;
	private static Stack stack = new Stack();

	static {
		print("Initialising " + Scope.GLOBAL_SCOPE.getScopeName() + "-scope.");
		stack.appendScope(Scope.GLOBAL_SCOPE.getScopeName());
	}

	public static int countOfScopes() {
		return stack.height();
	}

	public static void deleteScope(Scope scope) {
		ScopeMemory deleted = stack.popScope(scope.getScopeName());
		if (deleted.getScope().containsKey(String.valueOf((char) (counterName - 1))))
			counterName--;
		print("-- Deleted " + scope.getScopeName() + " --");
	}

	/**
	 * Finds a Variable by its name.
	 * 
	 * @param name         is the name of the searched var.
	 * @param calledInLine is the line in which the call takes place.
	 */
	public static Variable get(String name, int calledInLine) {
		return stack.findVar(name, calledInLine);
	}

	public static void initCounter(Scope scope, long value, int calledInLine) {
		Variable cnt = new Variable(scope.getStart(), DataType.NUMBER);
		cnt.initialise(new Name(String.valueOf(counterName), scope.getStart()), new NumberValue(value));
		if (counterName > (FIRST_COUNTER_NAME + LOOP_VAR_COUNT))
			throw new IllegalCodeFormatException(calledInLine, "Nesting more than " + (LOOP_VAR_COUNT + 1) + " loops is forbidden.");
		counterName++;
	}

	public static void nameCheck(String name, int calledInLine) {
		for (byte b = 0; b < LOOP_VAR_COUNT; b++)
			if (String.valueOf((char) (FIRST_COUNTER_NAME + b)).equals(name))
				throw new DeclarationException(calledInLine, "Variable cannot be manually declared with a counter-name. ("
						+ FIRST_COUNTER_NAME + "-" + (char) (FIRST_COUNTER_NAME + LOOP_VAR_COUNT) + ") was " + name);
		if (KeywordFinder.isKeyword(name) || DataType.isType(name))
			throw new IllegalArgumentException("A Variable cannot be named after a keyword or a type.");
	}

	public static void registerScope(Scope scope) {
		stack.appendScope(scope.getScopeName());
		print("-- Registered " + scope.getScopeName() + " --");
	}

	public static void registerVar(Variable var) {
		stack.registerVar(var);
	}

}
