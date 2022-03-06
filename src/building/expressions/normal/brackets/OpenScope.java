package building.expressions.normal.brackets;

import static building.types.specific.BuilderType.CLOSE_SCOPE;
import static building.types.specific.BuilderType.OPEN_SCOPE;

import java.util.List;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ScopeBracket;
import building.expressions.normal.BuilderExpression;
import misc.main.Main;

public final class OpenScope extends Expression implements ScopeBracket {

	private final int myMatch;

	public OpenScope(int lineID) {
		super(lineID, OPEN_SCOPE);
		long brack = 1;
		for (int i = lineIdentifier + 1; i < Main.PROGRAM.size(); i++) {
			List<BuilderExpression> exp = Main.PROGRAM.getLine(i).getExpressions();
			for (BuilderExpression e : exp) {
				if (e.is(OPEN_SCOPE))
					brack++;
				if (e.is(CLOSE_SCOPE)) {
					brack--;
					if (brack == 0) {
						myMatch = e.lineIdentifier;
						return;
					}
				}
			}
		}
		throw new AssertionError("Found no matching CloseScope.");
	}

	@Override
	public int getMatch() {
		return myMatch;
	}
}
