package building.expressions.abstractions.scopes;

import java.util.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.main.blueprints.*;
import building.expressions.main.functions.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import errorhandeling.*;
import misc.util.*;

public class ScopeManager {
	
	public static final BlueprintSave<Definition> DEFS = new BlueprintSave<>();
	public static final BlueprintSave<Variable> ATTRIBUTES = new BlueprintSave<>();
	
	/**
	 * Searches the {@link #STACK} and then the {@link #ATTRIBUTES} for the variable.
	 *
	 * @param name is the {@link Name} of the {@link Variable}.
	 * @return the {@link Variable} or null.
	 */
	public static Variable getVar(Name name) {
		Variable var = STACK.find(name);
		return var != null ? var : ATTRIBUTES.find(name.getBlueprintPath().blueprint, new ID<>(name.toString()));
	}
	
	/** Returns the "lowest" unused counter-name in this {@link Blueprint}. */
	public static Name getCounterName(Blueprint blueprint) {
		// TODO
		return null;
	}
	
	public static class BlueprintSave<T extends Identifieable<?>> {
		
		private final Map<Blueprint, HashMap<ID<T>, T>> content = new HashMap<>();
		
		@SuppressWarnings("unchecked")
		public void register(Blueprint blueprint, T value) {
			HashMap<ID<T>, T> bpMap = content.get(blueprint);
			if (bpMap == null) {
				bpMap = new HashMap<>();
				content.put(blueprint, bpMap);
			}
			assert bpMap.put((ID<T>) value.generateID(), value) == null : value.getClass().getSimpleName() + " should already be filtered.";
		}
		
		public T find(Blueprint blueprint, ID<T> id) {
			Map<ID<T>, T> bpMap = content.get(blueprint);
			if (bpMap == null)
				throw new PseudocodeException("Blueprint", //
						"The Blueprint " + blueprint + " was never registered with any values.", //
						blueprint.getBlueprintPath());
			return bpMap.get(id);
		}
	}
	
	public static final ScopeStack STACK = new ScopeStack();
	
	/** Every {@link Variable} that gets defined in a {@link ScopeHolder}. */
	public static class ScopeStack {
		
		private final Stack<Scope> STACK = new Stack<>();
		
		/** Allocates a new {@link Scope} on this {@link ScopeStack}. */
		public void allocate() {
			if (STACK.isEmpty())
				STACK.push(new Scope());
			else
				STACK.peek().emptyframes++;
		}
		
		/** Frees the top {@link Scope} on this {@link ScopeStack}. */
		public void free() {
			if (STACK.peek().emptyframes == 0)
				STACK.pop();
			else
				STACK.peek().emptyframes--;
		}
		
		/** Registers a {@link Variable} in the top {@link Scope} on this {@link ScopeStack}. */
		public void register(Variable var) {
			Scope tos = STACK.peek();
			if (tos.emptyframes == 0)
				tos.scope.put(var.getNameString(), var);
			else {
				tos.emptyframes--;
				tos = new Scope();
				STACK.push(tos);
				tos.scope.put(var.getNameString(), var);
			}
		}
		
		/** Finds a {@link Variable} on this {@link ScopeStack} (top to bottom) */
		public Variable find(Name n) {
			for (Scope frame : STACK) {
				Variable var = frame.scope.get(n.getNameString());
				if (var != null)
					return var;
			}
			throw new PseudocodeException("VarNotFound", "There is no variable called " + n + ".", n.getBlueprintPath());
		}
		
		@Override
		public String toString() {
			return STACK.toString();
		}
		
		/** This is a {@link Scope} that just contains Variables. */
		private class Scope {
			
			/** The amount of empty nested scopes in this. */
			int emptyframes = 0;
			
			/** Every {@link Variable} in this {@link Scope}, identified by its {@link Name}. */
			final Map<String, Variable> scope = new HashMap<>();
			
			@Override
			public String toString() {
				return emptyframes == 0 ? scope.toString() : "[" + emptyframes + "]";
			}
		}
	}
}