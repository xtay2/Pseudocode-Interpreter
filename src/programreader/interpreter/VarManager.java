package programreader.interpreter;

import java.util.ArrayList;
import java.util.HashMap;

import programreader.expressions.normal.Variable;
import programreader.expressions.special.Scope;
import static helper.Output.*;

public final class VarManager {

	private static Stack stack = new Stack();

	static {
		print("Initialising " + Scope.GLOBAL_SCOPE.getScopeName() + "-scope.");
		stack.appendScope(Scope.GLOBAL_SCOPE.getScopeName());
	}

	public static void registerScope(Scope scope) {
		stack.appendScope(scope.getScopeName());
		print("-- Registered " + scope.getScopeName() + " --");
	}

	public static void registerVar(Variable var) {
		stack.registerVar(var);
	}

	public static Variable get(String name) {
		return stack.findVar(name);
	}

	public static void deleteScope(Scope scope) {
		stack.popScope(scope.getScopeName());
		print("-- Deleted " + scope.getScopeName() + " --");
	}

	public static int countOfScopes() {
		return stack.height();
	}

}

final class Stack {

	private final ArrayList<ScopeMemory> scopes = new ArrayList<ScopeMemory>();

	public void appendScope(String scopeName) {
		scopes.add(new ScopeMemory(scopeName));
	}

	private HashMap<String, Variable> peekScope() {
		return scopes.get(height() - 1).getScope();
	}

	public void popScope(String name) {
		if (!name.equals(scopes.get(height() - 1).getName()))
			throw new IllegalArgumentException(
					"Trying to delete non-top-scope " + name + "\ntop-scope was " + scopes.get(height() - 1).getName() + "\n" + this);
		scopes.remove(height() - 1);
	}

	public void registerVar(Variable var) {
		if (peekScope().containsValue(var))
			throw new IllegalArgumentException("Var \"" + var.getName() + "\" couldn't be registered in scope \""
					+ scopes.get(height() - 1).getScope() + "\", as it exists here already.");
		peekScope().put(var.getName(), var);
	}

	public Variable findVar(String varName) {
		for (int i = scopes.size() - 1; i >= 0; i--) {
			HashMap<String, Variable> scope = scopes.get(i).getScope();
			Variable var = scope.get(varName);
			if (var != null)
				return var;
		}
		throw new IllegalArgumentException("Var " + varName + " doesn't exist.");
	}

	public int height() {
		return scopes.size();
	}

	@Override
	public String toString() {
		return scopes.toString();
	}
}

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
