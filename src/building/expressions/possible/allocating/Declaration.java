package building.expressions.possible.allocating;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Variable;
import building.types.specific.DataType;
import building.types.specific.FlagType;
import interpreting.modules.interpreter.Interpreter;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

public class Declaration extends Allocating implements Flaggable {

	private final Set<FlagType> flags = new HashSet<>();

	/**
	 * Creates an {@link Declaration}.
	 * 
	 * @param type   shouldn't be null.
	 * @param target shouldn't be null.
	 * @param val    shouldn't be null.
	 */
	public Declaration(int lineID, DataType type, ValueChanger target, ValueHolder val) {
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
		if (v instanceof ArrayValue av)
			av.init();
		new Variable(lineIdentifier, getScope(), (DataType) type, target.getName(), v).addFlags(flags);
		return v;
	}

	@Override
	public void addFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}

}
