package expressions.main.functions;

import static types.specific.KeywordType.MAIN;

import expressions.abstractions.ScopeHolder;
import expressions.normal.brackets.OpenScope;
import modules.interpreter.FuncManager;
import types.specific.KeywordType;

public class MainFunction extends ScopeHolder {

	public MainFunction(int lineID, OpenScope os) {
		super(lineID, MAIN, os);
		FuncManager.registerFunction(KeywordType.MAIN.keyword, lineID);
	}

	@Override
	public boolean execute() {
		callFirstLine();
		getScope().clear();
		return false;
	}
}
