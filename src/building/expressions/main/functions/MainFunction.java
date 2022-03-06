package building.expressions.main.functions;

import static building.types.specific.KeywordType.MAIN;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.normal.brackets.OpenScope;
import building.types.specific.KeywordType;
import interpreting.modules.interpreter.FuncManager;

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
