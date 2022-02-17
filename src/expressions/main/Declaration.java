package expressions.main;

import static expressions.abstractions.GlobalScope.GLOBAL;

import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.Scope;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Variable;
import interpreter.VarManager;
import types.ExpressionType;

public class Declaration extends MainExpression implements MergedExpression {

	private Variable var;
	private ValueHolder val;

	public Declaration(int line) {
		super(line, ExpressionType.MERGED);
	}

	/** [VARIABLE] [VALUEHOLDER] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a Declaration has to contain a Variable and a ValueHolder.");
		var = (Variable) e[0];
		val = (ValueHolder) e[1];
	}

	/** Executes this Declaration and calls the next line afterwards. */
	@Override
	public boolean execute(ValueHolder... params) {
		initAndRegister();
		return callNextLine();
	}

	/**
	 * Registers and initialises this variable if it is in the {@link Scope#GLOBAL_SCOPE}.
	 */
	public void registerIfGlobal() {
		if (var.getScope() == GLOBAL)
			initAndRegister();
	}

	/**
	 * Initialises the {@link Variable} with its value and registers it at the {@link VarManager}.
	 */
	private void initAndRegister() {
		VarManager.registerVar(var);
		var.setValue(val.getValue());
	}
}
