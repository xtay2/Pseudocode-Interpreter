package expressions.normal.brackets;

import static parsing.program.ExpressionType.*;

import expressions.normal.Expression;
import expressions.special.Bracket;

public class CloseBracket extends Expression implements Bracket {

	private OpenBracket match;

	public CloseBracket(int line) {
		super(line);
		setExpectedExpressions(CLOSE_BRACKET, EXPECTED_RETURN_TYPE, COMMA, OPEN_SCOPE, INFIX_OPERATOR, ARRAY_END);
	}

	@Override
	public Bracket getMatch() {
		return match;
	}

	@Override
	public void setMyMatch(Bracket match) {
		this.match = (OpenBracket) match;
	}
}
