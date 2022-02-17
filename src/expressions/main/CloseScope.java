package expressions.main;

import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.ScopeBracket;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import main.Main;
import types.ExpressionType;

public final class CloseScope extends MainExpression implements ScopeBracket {

	private OpenScope myMatch;

	/**
	 * Finds the matching {@link OpenScope} after getting constructed.
	 */
	public CloseScope(int line) {
		super(line, ExpressionType.CLOSE_SCOPE);
		findMatchingOpenScope();
	}

	/**
	 * Should only get called by the constructor of this.
	 */
	@Deprecated
	private final void findMatchingOpenScope() {
		int brack = -1;
		for (int i = lineIdentifier - 1; i >= 0; i--) {
			MainExpression m = Main.PROGRAM.getLine(i).getMainExpression();
			if (m instanceof CloseScope)
				brack--;
			if (m instanceof ScopeBracket b) {
				brack++;
				if (brack == 0) {
					b.setMyMatch(this);
					return;
				}
			}
		}
		throw new AssertionError("No-Scope-Exception, höhö. Der Formatter ist kaputt.");
	}

	@Override
	public boolean execute(ValueHolder... params) {
		return true; // Just go back
	}

	@Override
	public ScopeBracket getMatch() {
		return myMatch;
	}

	@Override
	public void setMyMatch(ScopeBracket match) {
		myMatch = (OpenScope) match;
	}
}
