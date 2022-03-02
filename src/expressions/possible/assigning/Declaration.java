package expressions.possible.assigning;

import java.util.HashSet;
import java.util.Set;

import datatypes.Value;
import exceptions.parsing.UnexpectedFlagException;
import expressions.abstractions.Scope;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import expressions.normal.flag.Flaggable;
import modules.interpreter.Interpreter;
import types.specific.FlagType;
import types.specific.data.ExpectedType;

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
		Variable.quickCreate(lineIdentifier, getScope(), (ExpectedType) type, (Name) target, v, flags.toArray(new FlagType[flags.size()]));
		return v;
	}

	@Override
	public void setFlags(Set<FlagType> flags) throws UnexpectedFlagException {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}

}
