package expressions.normal;

import expressions.special.Bracket;
import expressions.special.Expression;
import parser.program.ExpressionType;

public class OpenBracket extends Expression implements Bracket {

	private CloseBracket match;

	public OpenBracket(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.CLOSE_BRACKET, ExpressionType.EXPECTED_TYPE, ExpressionType.LITERAL, ExpressionType.NAME);
	}

	@Override
	public Bracket getMatch() {
		return match;
	}

	@Override
	public void setMyMatch(Bracket match) {
		//TODO : Implement me!
	}
}
