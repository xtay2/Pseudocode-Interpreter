package expressions.main.functions;

import expressions.abstractions.Expression;
import expressions.abstractions.Scope;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import types.ExpressionType;
import types.specific.KeywordType;

public class MainFunction extends ScopeHolder {

	private Scope scope = null;

	public MainFunction(int lineID) {
		super(lineID, KeywordType.MAIN, ExpressionType.OPEN_SCOPE);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		scope.reg();
		callFirstLine();
		scope.del();
		return false;
	}

	/** [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 1)
			throw new AssertionError("Only the open-scope is needed.");
		initScope((OpenScope) e[0]);
	}
}
