package building.expressions.abstractions;

import static misc.helper.Output.print;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.main.CloseBlock;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.types.specific.DataType;
import building.types.specific.FlagType;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.numerical.NumberValue;
import runtime.exceptions.VarNotFoundException;

/**
 * A Scope is a Block that limits the visibility of variables.
 * 
 * Every {@link ScopeHolder} contains a Scope.
 * 
 * @see OpenBlock
 * @see CloseBlock
 */
public class Scope {

	private final String scopeName;

	private final Scope lowerScope;

	private final int lineID;

	protected final Map<String, Registerable> memory = new HashMap<>();

	/**
	 * Constructs a Scope and connects both Scope-Brackets.
	 * 
	 * @param scopeName  is the name of this scope. This class adds the lineIDs to make it unique.
	 * @param os         is the {@link OpenBlock} bracket. If null, the scope gets reduced to the
	 *                   lineID.
	 * @param cs         is the matching {@link CloseBlock}.
	 * @param lowerScope is the scope this one is in.
	 */
	public Scope(int lineID, String scopeName, Scope lowerScope) {
		this.lineID = lineID;
		this.scopeName = scopeName + lineID;
		this.lowerScope = lowerScope;
		if (scopeName == null)
			throw new AssertionError("The scope-name cannot be null.");
	}

	/**
	 * Protected Constructor for the {@link GlobalScope}.
	 */
	protected Scope() {
		lineID = -1;
		this.scopeName = GlobalScope.NAME;
		this.lowerScope = null;
	}

	/** Returns a universal identifying name for every scope. */
	public final String getScopeName() {
		return scopeName;
	}

	/** Registers the {@link Registerable} in this scope. */
	public final void register(Registerable reg) {
		if (!contains(reg.getNameString())) {
			memory.put(reg.getNameString(), reg);
			print("Registered " + reg.getNameString() + " in line " + reg.getOriginalLine());
		} else
			throw new IllegalCodeFormatException(reg.getOriginalLine(),
					reg.getNameString() + " is already defined in line " + get(reg.getNameString()).getOriginalLine());
	}

	/** Returns true, if this, or any underlying Scope contains the quested {@link Registerable}. */
	public final boolean contains(String target) {
		if (memory.containsKey(target))
			return true;
		if (lowerScope != null)
			return lowerScope.contains(target);
		return false;
	}

	/** Deletes all registered {@link Registerable} from this scope. */
	public final void clear() {
		memory.clear();
	}

	/** Initialises and registers a Counter-Variable with the given value. */
	public final void initCounter(NumberValue i, Scope scope, int originalLine) {
		new Variable(originalLine, scope, DataType.NUMBER, getCounterName(originalLine), i).addFlags(Set.of(FlagType.CONSTANT));
	}

	// GETTERS--------------------------------------------------------------------

	/** Returns the quested {@link Variable} from this, or any underlying scope. */
	public final Variable getVar(String varName, int orgLine) {
		Registerable r = get(varName);
		if (r == null) {
			throw new VarNotFoundException(orgLine,
					"Couldn't find var \"" + varName + "\".\nThis scope \"" + getScopeName() + "\" contains: " + wholeMemToString());
		}
		if (r instanceof Variable var)
			return var;
		throw new VarNotFoundException(orgLine, varName + " was found in scope \"" + getScopeName() + "\" but is not a function.");
	}

	/**
	 * Searches all underlying Scopes for the quested name.
	 * 
	 * @return the {@link Registerable} if found, or null if not.
	 */
	protected final Registerable get(String target) {
		Scope s = this;
		do {
			Registerable r = s.memory.get(target);
			if (r != null)
				return r;
			s = s.lowerScope;
		} while (s != null);
		return null;
	}

	// HELPER-FUNCTIONS-------------------------------------------------------

	/**
	 * Returns a {@link Map} of every {@link Registerable} thats saved in this and all underlying
	 * scopes.
	 */
	private Set<Registerable> getWholeMem() {
		if (lowerScope == null)
			return new HashSet<>(memory.values());
		HashSet<Registerable> mem = new HashSet<>();
		mem.addAll(lowerScope.getWholeMem());
		mem.addAll(memory.values());
		return mem;
	}

	/**
	 * Returns the {@link String}-representation of the whole memory.
	 * 
	 * Gets used by errormessages in {@link #getVar(String, int)} and {@link #getFunc(String, int)}.
	 */
	private final String wholeMemToString() {
		StringBuilder wholeMem = new StringBuilder();
		getWholeMem().stream().forEach(e -> wholeMem.append("\n-\"" + e.getNameString() + "\"   in " + e.getScope().getScopeName()));
		return wholeMem.toString();
	}

	/**
	 * Returns the next available counter-name. If more than 8 counter-names are in use, an
	 * {@link IllegalCodeFormatException} gets thrown.
	 */
	private final Name getCounterName(int originalLine) {
		final char fstCnt = 'i';
		final int cntNr = 8;
		for (char c = fstCnt; c < (fstCnt + cntNr); c++) {
			String n = String.valueOf(c);
			if (!contains(n))
				return new Name(lineID, n);
		}
		throw new IllegalCodeFormatException(originalLine,
				"Too many nested Scopes! All " + cntNr + " counternames " + fstCnt + "-" + (char) (fstCnt + cntNr - 1) + " are in use.");
	}
}