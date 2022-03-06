package building.expressions.possible.allocating;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Variable;
import building.expressions.normal.flag.Flaggable;
import building.types.specific.FlagType;
import building.types.specific.data.ExpectedType;
import interpreting.modules.interpreter.Interpreter;
import runtime.datatypes.Value;

public class Declaration extends Allocating implements Flaggable {

	private final Set<FlagType> flags = new HashSet<>();

	/**
	 * Creates an {@link Declaration}.
	 * 
	 * @param type   shouldn't be null.
	 * @param target shouldn't be null.
	 * @param val    shouldn't be null.
	 */
	public Declaration(int lineID, ExpectedType type, ValueChanger target, ValueHolder val) {
		super(lineID, type, target, val);
	}

	/**
	 * Initialises the {@link Variable} with its value and registers it in its {@link Scope}.
	 * 
	 * Gets called by {@link Interpreter#registerGlobalVars}
	 */
	@Override
	public Value getValue() {
		Value v = val.getValue();
		Variable.quickCreate(lineIdentifier, getScope(), (ExpectedType) type, target.getName(), v,
				flags.toArray(new FlagType[flags.size()]));
		return v;
	}

	@Override
	public void setFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}

}
