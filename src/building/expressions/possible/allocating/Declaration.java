package building.expressions.possible.allocating;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import interpreting.modules.interpreter.*;
import runtime.datatypes.*;

/**
 * The {@link Expression} in code, that initialises {@link Variable}s.
 *
 * {@link ArrayVariable}s are getting inititlised by {@link ArrayDeclaration}s.
 */
public class Declaration extends Allocating implements Flaggable {
	
	protected final Set<FlagType> flags = new HashSet<>();
	private final DataType datatype;
	
	/**
	 * Creates an {@link Declaration}.
	 *
	 * @param type shouldn't be null.
	 * @param target shouldn't be null.
	 * @param val shouldn't be null.
	 */
	public Declaration(int lineID, DataType datatype, Name target, ValueHolder val) {
		super(lineID, BuilderType.MERGED, target, val);
		this.datatype = datatype;
	}
	
	/**
	 * Initialises the {@link Variable} with its value and registers it in its {@link Scope}.
	 *
	 * Gets called by {@link Interpreter#registerGlobalVars}
	 */
	@Override
	public Value getValue() {
		Value v = val.getValue();
		new Variable(lineIdentifier, datatype, (Name) target, v).addFlags(flags);
		return v;
	}
	
	@Override
	public void addFlags(Set<FlagType> flags) {
		flags.addAll(flags);
	}
	
	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}
	
}
