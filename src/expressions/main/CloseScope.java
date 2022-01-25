package expressions.main;

import expressions.normal.brackets.OpenScope;
import expressions.special.Bracket;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import main.Main;

public class CloseScope extends MainExpression implements Bracket {

	private OpenScope myMatch;

	/**
	 * Finds the matching {@link OpenScope} after getting constructed.
	 */
	public CloseScope(int line) {
		super(line);
		setExpectedExpressions();
		findMatchingOpenScope();
	}

	/**
	 * Should only get called by the constructor of this.
	 */
	private final void findMatchingOpenScope() {
		int brack = -1;
		for (int i = lineIdentifier - 1; i >= 0; i--) {
			MainExpression m = Main.PROGRAM.getLine(i).getMainExpression();
			if (m instanceof CloseScope)
				brack--;
			if (m instanceof Scope s) {
				brack++;
				if (brack == 0) {
					s.connectScopeEnd(this);
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
	public Bracket getMatch() {
		return myMatch;
	}

	@Override
	public void setMyMatch(Bracket match) {
		myMatch = (OpenScope) match;
	}
}
