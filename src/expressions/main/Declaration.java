package expressions.main;

import java.util.Set;

import exceptions.parsing.UnexpectedFlagException;
import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import expressions.normal.flag.Flaggable;
import modules.interpreter.Interpreter;
import modules.interpreter.VarManager;
import types.specific.DataType;
import types.specific.FlagType;

public class Declaration extends MainExpression implements MergedExpression, Flaggable {

	private Name name;
	private ValueHolder val;
	private Set<FlagType> flags;

	public Declaration(int line, DataType type) {
		super(line, type);
	}

	/** [Name] [VALUEHOLDER] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a Declaration has to contain a Variable and a ValueHolder.");
		name = (Name) e[0];
		val = (ValueHolder) e[1];
	}

	/** Executes this Declaration and calls the next line afterwards. */
	@Override
	public boolean execute(ValueHolder... params) {
		initAndRegister();
		return callNextLine();
	}

	/**
	 * Initialises the {@link Variable} with its value and registers it at the {@link VarManager}.
	 * 
	 * Gets called by {@link Interpreter#registerGlobalVars}
	 */
	public void initAndRegister() {
		Variable.quickCreate(lineIdentifier, (DataType) type, name, val.getValue(), flags.toArray(new FlagType[flags.size()]));
	}

	@Override
	public void setFlags(Set<FlagType> flags) throws UnexpectedFlagException {
		this.flags = flags;
	}
}
