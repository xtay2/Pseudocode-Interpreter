package expressions.main;

import expressions.normal.Expression;
import expressions.normal.Variable;
import expressions.special.MergedExpression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.VarManager;

public class Declaration extends MainExpression implements MergedExpression {

	private Variable var;
	private ValueHolder val;

	public Declaration(int line) {
		super(line);
	}

	/** [VARIABLE] [VALUEHOLDER] */
	@Override
	public void merge(Expression... e) {
		if(e.length != 2)
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
	 * Registers and initialises this variable if it is in the
	 * {@link Scope#GLOBAL_SCOPE}.
	 */
	public void registerIfGlobal() {
		if (var.getScope() == Scope.GLOBAL_SCOPE)
			initAndRegister();
	}

	/**
	 * Initialises the {@link Variable} with its value and registers it at the
	 * {@link VarManager}.
	 */
	private void initAndRegister() {
		VarManager.registerVar(var);
		var.setValue(val.getValue());
	}
}
