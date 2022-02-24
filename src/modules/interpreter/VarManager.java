package modules.interpreter;

import static expressions.abstractions.GlobalScope.GLOBAL;
import static helper.Output.print;
import static types.specific.data.DataType.NUMBER;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import datatypes.numerical.NumberValue;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalCallException;
import expressions.abstractions.Scope;
import expressions.main.loops.Loop;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import types.specific.FlagType;

public abstract class VarManager {

	private static final char FIRST_COUNTER_NAME = 'i';
	private static char counterName = FIRST_COUNTER_NAME;
	private static final byte LOOP_VAR_COUNT = 7;

	static {
		print("Initialising " + GLOBAL.getScopeName() + "-scope.");
		registerScope(GLOBAL);
	}

	/** Returns the number of Scopes. (Stack Height.) */
	public static int countOfScopes() {
		return Stack.height();
	}

	/** Register a Scope, so that Variables can be registered in it. */
	public static void registerScope(Scope scope) {
		Stack.appendScope(scope.getScopeName());
	}

	/** Deletes a Scope, and all Variables in it. */
	public static void deleteScope(Scope scope) {
		ScopeMemory deleted = Stack.popScope(scope.getScopeName());
		if (deleted.getScope().containsKey(String.valueOf((char) (counterName - 1))))
			counterName--;
	}

	/** Registers a Variable on the stack. */
	public static void registerVar(Variable var) {
		nameCheck(var.getName(), var.getOriginalLine());
		Stack.registerVar(var);
	}

	/**
	 * Finds a Variable by its name.
	 * 
	 * @param name         is the name of the searched var.
	 * @param calledInLine is the line in which the call takes place.
	 */
	public static Variable get(String name, int calledInLine) {
		return Stack.findVar(name, calledInLine);
	}

	/**
	 * Initialises a counter of any {@link Loop} as a {@link Variable}.
	 * 
	 * @param scope     is the scope of the {@link Loop}.
	 * @param iteration is the value of the {@link Variable}.
	 */
	public static void initCounter(Scope scope, NumberValue iteration) {
		int calledInLine = scope.getStart();
		Name varName = new Name(calledInLine, String.valueOf(counterName));
		//// Cannot use quickCreate because of the Name-Check ////
		Variable counter = new Variable(calledInLine, NUMBER);
		counter.merge(varName, iteration);
		counter.setFlags(Set.copyOf(List.of(FlagType.CONSTANT)));
		Stack.registerVar(counter);
		////////////////////////////////////////////////////////
		if (counterName > (FIRST_COUNTER_NAME + LOOP_VAR_COUNT))
			throw new IllegalCodeFormatException(calledInLine, "Nesting more than " + (LOOP_VAR_COUNT + 1) + " loops is forbidden.");
		counterName++;
	}

	/**
	 * Checks the name of every var that the user tries to register.
	 * 
	 * This gets called in {@link #registerVar(Variable)}
	 * 
	 * @param name         cannot be a loop-var, a keyword or a data-type.
	 * @param calledInLine is the original line of the var (for errors.)
	 */
	private static void nameCheck(String name, int calledInLine) {
		for (byte b = 0; b < LOOP_VAR_COUNT; b++)
			if (String.valueOf((char) (FIRST_COUNTER_NAME + b)).equals(name))
				throw new DeclarationException(calledInLine, "Variable cannot be manually declared with a counter-name. ("
						+ FIRST_COUNTER_NAME + "-" + (char) (FIRST_COUNTER_NAME + LOOP_VAR_COUNT) + ") was " + name);
		if (!Name.isName(name))
			throw new IllegalArgumentException("A Variable has to be alphanumerical and cannot be named after a keyword or a type.");
	}

}

/**
 * Cashes all Variables for the current scope.
 */
final class ScopeMemory {

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

/**
 * The Stack saves every {@link ScopeMemory} and with that, every {@link Variable}.
 */
abstract class Stack {

	private static final ArrayList<ScopeMemory> scopes = new ArrayList<>();

	private Stack() {
		// A Stack cannot be constructed, as it is purely static.
	}

	public static void appendScope(String scopeName) {
		scopes.add(new ScopeMemory(scopeName));
	}

	public static Variable findVar(String varName, int calledInLine) {
		for (int i = scopes.size() - 1; i >= 0; i--) {
			HashMap<String, Variable> scope = scopes.get(i).getScope();
			Variable var = scope.get(varName);
			if (var != null)
				return var;
		}
		throw new IllegalCallException(calledInLine, "Called var " + varName + " doesn't exist. \nScopes: " + scopes);
	}

	public static int height() {
		return scopes.size();
	}

	private static HashMap<String, Variable> peekScope() {
		return scopes.get(height() - 1).getScope();
	}

	public static ScopeMemory popScope(String name) {
		if (!name.equals(scopes.get(height() - 1).getName()))
			throw new IllegalArgumentException("Trying to delete non-top-scope " + name + "\ntop-scope was "
					+ scopes.get(height() - 1).getName() + "\n" + scopes.toString());
		return scopes.remove(height() - 1);
	}

	public static void registerVar(Variable var) {
		if (peekScope().containsValue(var))
			throw new IllegalArgumentException("Var \"" + var.getName() + "\" couldn't be registered in scope \""
					+ scopes.get(height() - 1).getScope() + "\", as it exists here already.");
		peekScope().put(var.getName(), var);
	}
}
