package expressions.main.functions;

import expressions.abstractions.Expression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import types.specific.ExpressionType;
import types.specific.KeywordType;

public class MainFunction extends ScopeHolder {

	public MainFunction(int lineID) {
		super(lineID, KeywordType.MAIN, ExpressionType.OPEN_SCOPE);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		callFirstLine();
		getScope().clear();
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
