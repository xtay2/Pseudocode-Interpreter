package building.expressions.possible.allocating;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.types.specific.FlagType;
import building.types.specific.datatypes.DataType;
import interpreting.modules.interpreter.Interpreter;
import runtime.datatypes.Value;

/**
 * The {@link Expression} in code, that initialises {@link Variable}s.
 * 
 * {@link ArrayVariable}s are getting inititlised by {@link ArrayDeclaration}s.
 */
public class Declaration extends Allocating implements Flaggable {

	protected final Set<FlagType> flags = new HashSet<>();

	/**
	 * Creates an {@link Declaration}.
	 * 
	 * @param type   shouldn't be null.
	 * @param target shouldn't be null.
	 * @param val    shouldn't be null.
	 */
	public Declaration(int lineID, DataType type, Name target, ValueHolder val) {
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
		new Variable(lineIdentifier, getScope(), (DataType) type, (Name) target, v).addFlags(flags);
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
