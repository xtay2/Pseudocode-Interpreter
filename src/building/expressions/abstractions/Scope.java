package building.expressions.abstractions;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.main.CloseBlock;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.expressions.possible.Call;
import interpreting.exceptions.IllegalCodeFormatException;
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

	/**
	 * The top of the stack. Gets incremented with every {@link Call} and decremented after every
	 * return, end of function. The tos is part of every local variable that gets declared. This allows
	 * for recursion.
	 */
	public static int tos = 0;

	private final String scopeName;

	private final Scope lowerScope;

	private final int lineID;

	protected final Set<UVID> memory = new HashSet<>();

	/**
	 * Unique-Variable-Identification.
	 */
	protected record UVID(String varName, int varID, Registerable reg) implements Comparable<UVID> {
		@Override
		public int compareTo(UVID o) {
			return Integer.compare(varID, o.varID);
		}
	}

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
		String regName = reg.getNameString();
		if (!contains(regName, tos)) {
			memory.add(new UVID(regName, tos, reg));
		} else
			throw new IllegalCodeFormatException(reg.getOriginalLine(),
					regName + " is already defined in line " + get(reg.getNameString()).getOriginalLine());
	}

	/** Returns true, if this, or any underlying Scope contains the quested {@link Registerable}. */
	public final boolean containsAny(final String target) {
		if (memory.stream().anyMatch(e -> e.varName.equals(target)))
			return true;
		if (lowerScope != null)
			return lowerScope.containsAny(target);
		return false;
	}

	/**
	 * Returns true, if this, or any underlying Scope contains the quested {@link Registerable} with the
	 * specified {@link UVID#varID}.
	 */
	public final boolean contains(final String target, final int id) {
		if (memory.stream().anyMatch(e -> e.varName.equals(target) && e.varID == id))
			return true;
		if (lowerScope != null)
			return lowerScope.contains(target, id);
		return false;
	}

	/** Deletes the registered {@link UVID}s with the highest {@link UVID#varID} from this scope. */
	public final void clear() {
		HashMap<String, UVID> greatesMap = new HashMap<>();
		for (UVID uvid : memory) {
			UVID current = greatesMap.get(uvid.varName);
			if (current == null || current.varID < uvid.varID)
				greatesMap.put(uvid.varName, uvid);
		}
		memory.removeAll(greatesMap.values());
	}

	// GETTERS--------------------------------------------------------------------

	/** Returns the quested {@link Variable} from this, or any underlying scope. */
	public final Variable getVar(String target, int orgLine) {
		Registerable r = get(target);
		if (r == null) {
			throw new VarNotFoundException(orgLine,
					"Couldn't find var \"" + target + "\".\nThis scope \"" + getScopeName() + "\" contains: " + wholeMemToString());
		}
		if (r instanceof Variable var)
			return var;
		throw new VarNotFoundException(orgLine, target + " was found in scope \"" + getScopeName() + "\" but is not a function.");
	}

	/**
	 * Searches all underlying Scopes for the quested name.
	 * 
	 * @return the {@link Registerable} if found, or null if not.
	 */
	protected final Registerable get(String target) {
		Scope s = this;
		do {
			UVID u = s.memory.stream().filter(e -> e.varName.equals(target)).max(UVID::compareTo).orElseGet(() -> null);
			if (u != null)
				return u.reg;
			s = s.lowerScope;
		} while (s != null);
		return null;
	}

	// HELPER-FUNCTIONS-------------------------------------------------------

	/**
	 * Returns a {@link Map} of every {@link Registerable} thats saved in this and all underlying
	 * scopes.
	 */
	private Set<UVID> getWholeMem() {
		if (lowerScope == null)
			return new HashSet<>(memory);
		HashSet<UVID> mem = new HashSet<>();
		mem.addAll(lowerScope.getWholeMem());
		mem.addAll(memory);
		return mem;
	}

	/**
	 * Returns the {@link String}-representation of the whole memory.
	 * 
	 * Gets used by errormessages in {@link #getVar(String, int)} and {@link #getFunc(String, int)}.
	 */
	private final String wholeMemToString() {
		StringBuilder wholeMem = new StringBuilder();
		getWholeMem().stream().forEach(e -> wholeMem.append("\n-\"" + e.varName));
		return wholeMem.toString();
	}

	/**
	 * Returns the next available counter-name. If more than 8 counter-names are in use, an
	 * {@link IllegalCodeFormatException} gets thrown.
	 */
	public final Name getCounterName(int originalLine) {
		final char fstCnt = 'i';
		final int cntNr = 8;
		for (char c = fstCnt; c < (fstCnt + cntNr); c++) {
			String n = String.valueOf(c);
			if (!containsAny(n))
				return new Name(lineID, n);
		}
		throw new IllegalCodeFormatException(originalLine,
				"Too many nested Scopes! All " + cntNr + " counternames " + fstCnt + "-" + (char) (fstCnt + cntNr - 1) + " are in use.");
	}
}