package expressions.abstractions;

import static helper.Output.print;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import datatypes.numerical.NumberValue;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.VarNotFoundException;
import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import types.specific.FlagType;
import types.specific.data.DataType;

/**
 * A Scope limits the visibility of variables.
 * 
 * Every {@link ScopeHolder} contains a Scope.
 * 
 * @see OpenScope
 * @see CloseScope
 */
public class Scope {

	private final int openScope;

	private final int closeScope;

	private final String scopeName;

	private final Scope lowerScope;

	private final Map<String, Variable> variables = new HashMap<>();

	/**
	 * Constructs a Scope and connects both Scope-Brackets.
	 * 
	 * @param scopeName  is the name of this scope. This class adds the lineIDs to make it unique.
	 * @param os         is the {@link OpenScope} bracket. If null, the scope gets reduced to the
	 *                   lineID.
	 * @param cs         is the matching {@link CloseScope}.
	 * @param lowerScope is the scope this one is in.
	 */
	public Scope(String scopeName, OpenScope os, CloseScope cs, Scope lowerScope) {
		this.openScope = os.lineIdentifier;
		this.closeScope = cs.lineIdentifier + 1;
		this.scopeName = scopeName + getStart() + "-" + getEnd();
		this.lowerScope = lowerScope;
		if (scopeName == null || os == null || cs == null)
			throw new AssertionError("Neither the name nor Open- or Close-Scope can be null.");
	}

	/**
	 * Protected Constructor for the {@link GlobalScope}.
	 */
	protected Scope() {
		this.scopeName = GlobalScope.NAME;
		this.openScope = 0;
		this.closeScope = Integer.MAX_VALUE;
		this.lowerScope = null;
	}

	/** Returns a universal identifying name for every scope. */
	public final String getScopeName() {
		return scopeName;
	}

	/** Returns the start of this Scope. */
	public final int getStart() {
		return openScope;
	}

	/** Returns the end of this Scope. */
	public final int getEnd() {
		return closeScope;
	}

	/** Registers the Variable in this scope. */
	public final void register(Variable var) {
		if (!contains(var.getNameString()))
			variables.put(var.getNameString(), var);
		else
			throw new IllegalCodeFormatException(var.getOriginalLine(),
					"Duplicate VariableName for " + var.getName() + " in " + getScopeName() + ".");
	}

	/** Returns the quested Variable from this, or any underlying scope. */
	public final Variable get(String varName, int orgLine) {
		Scope s = this;
		do {
			Variable v = s.variables.get(varName);
			if (v != null)
				return v;
			s = s.lowerScope;
		} while (s != null);
		StringBuilder allVars = new StringBuilder();
		getAllVars().stream().forEach(var -> allVars.append("\n-\"" + var.getNameString() + "\"   in " + var.getScope().getScopeName()));
		throw new VarNotFoundException(orgLine,
				"Couldn't find \"" + varName + "\".\nThis scope \"" + getScopeName() + "\" contains: " + allVars);
	}

	/** Returns true, if this, or any underlying Scope contains the quested Variable. */
	public final boolean contains(String varName) {
		if (variables.containsKey(varName))
			return true;
		if (lowerScope != null)
			return lowerScope.contains(varName);
		return false;
	}

	/** Deletes all registered Variables from this scope. */
	public final void clear() {
		variables.clear();
		print("Cleared scope " + getScopeName());
	}

	/**
	 * Returns a {@link Map} of all Variables in this and all underlying scopes.
	 */
	private Set<Variable> getAllVars() {
		if (lowerScope == null)
			return new HashSet<>(variables.values());
		HashSet<Variable> vars = new HashSet<>();
		vars.addAll(lowerScope.getAllVars());
		vars.addAll(variables.values());
		return vars;
	}

	/** Initialises and registers a Counter-Variable with the given value. */
	public final void initCounter(NumberValue i, Scope scope, int originalLine) {
		Variable.quickCreate(originalLine, scope, DataType.NUMBER, getCounterName(originalLine), i, FlagType.CONSTANT);
	}

	/**
	 * Returns the next available counter-name. If more than 8 counter-names are in use, an
	 * {@link IllegalCodeFormatException} gets thrown.
	 */
	private Name getCounterName(int originalLine) {
		final char fstCnt = 'i';
		final int cntNr = 8;
		for (char c = fstCnt; c < (fstCnt + cntNr); c++) {
			String n = String.valueOf(c);
			if (!contains(n))
				return new Name(getStart(), n);
		}
		throw new IllegalCodeFormatException(originalLine,
				"Too many nested Scopes! All " + cntNr + " counternames " + fstCnt + "-" + (char) (fstCnt + cntNr - 1) + " are in use.");
	}
}