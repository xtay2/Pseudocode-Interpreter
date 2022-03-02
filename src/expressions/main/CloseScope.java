package expressions.main;

import static types.specific.BuilderType.CLOSE_SCOPE;

import expressions.abstractions.MainExpression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ScopeBracket;
import main.Main;

public final class CloseScope extends MainExpression implements ScopeBracket {

	private final int myMatch;

	public CloseScope(int lineID) {
		super(lineID, CLOSE_SCOPE);
		long brack = -1;
		for (int i = lineIdentifier - 1; i >= 0; i--) {
			MainExpression exp = Main.PROGRAM.getLine(i).getMainExpression();
			if (exp instanceof CloseScope)
				brack--;
			if (exp instanceof ScopeHolder) {
				brack++;
				if (brack == 0) {
					myMatch = exp.lineIdentifier;
					return;
				}
			}
		}
		throw new AssertionError("Found no matching OpenScope.");
	}

	@Override
	public boolean execute() {
		return true; // Just go back
	}

	@Override
	public int getMatch() {
		return myMatch;
	}
}
