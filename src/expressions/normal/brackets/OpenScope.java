package expressions.normal.brackets;

import static types.ExpressionType.CLOSE_SCOPE;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ScopeBracket;
import expressions.main.CloseScope;
import types.ExpressionType;

public final class OpenScope extends Expression implements ScopeBracket {

	private CloseScope myMatch;

	public OpenScope(int line) {
		super(line, ExpressionType.OPEN_SCOPE, CLOSE_SCOPE);
	}

	@Override
	public ScopeBracket getMatch() {
		if (myMatch == null)
			throw new IllegalCodeFormatException(getOriginalLine(), "Block has no matching end. Use a ; or a }.");
		return myMatch;
	}

	@Override
	public void setMyMatch(ScopeBracket match) {
		myMatch = (CloseScope) match;
	}
}
