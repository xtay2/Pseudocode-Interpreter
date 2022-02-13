package expressions.normal.brackets;

import static parsing.program.ExpressionType.CLOSE_SCOPE;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.main.CloseScope;
import expressions.special.Bracket;
import parsing.program.ExpressionType;

public class OpenScope extends Expression implements Bracket {

	private CloseScope myMatch;

	public OpenScope(int line) {
		super(line, ExpressionType.OPEN_SCOPE);
		setExpectedExpressions(CLOSE_SCOPE);
	}

	@Override
	public Bracket getMatch() {
		if (myMatch == null)
			throw new IllegalCodeFormatException(getOriginalLine(), "Block has no matching end. Use a ; or a }.");
		return myMatch;
	}

	@Override
	public void setMyMatch(Bracket match) {
		myMatch = (CloseScope) match;
	}
}
