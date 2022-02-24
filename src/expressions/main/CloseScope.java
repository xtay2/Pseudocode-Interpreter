package expressions.main;

import java.util.List;

import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.ScopeBracket;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import main.Main;
import types.specific.ExpressionType;

public final class CloseScope extends MainExpression implements ScopeBracket {

	private final OpenScope myMatch;

	/**
	 * Finds the matching {@link OpenScope} after getting constructed.
	 */
	public CloseScope(int line) {
		super(line, ExpressionType.CLOSE_SCOPE);
		long brack = -1;
		for (int i = line - 1; i >= 0; i--) {
			List<Expression> exp = Main.PROGRAM.getLine(i).getExpressions();
			for (Expression e : exp) {
				if (e instanceof CloseScope)
					brack--;
				if (e instanceof OpenScope o) {
					brack++;
					if (brack == 0) {
						myMatch = o;
						o.setMyMatch(this);
						return;
					}
				}
			}
		}
		throw new AssertionError("Found no matching OpenScope.");
	}

	@Override
	public boolean execute(ValueHolder... params) {
		return true; // Just go back
	}

	@Override
	public ScopeBracket getMatch() {
		return myMatch;
	}
}
