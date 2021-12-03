package expressions.normal;

import expressions.special.Bracket;
import expressions.special.Expression;
import parser.program.ExpressionType;

public class CloseBracket extends Expression implements Bracket {

	private OpenBracket match;

	public CloseBracket(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.CLOSE_BRACKET, ExpressionType.ONE_LINE_STATEMENT, ExpressionType.EXPECTED_RETURN_TYPE,
				ExpressionType.COMMA, ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR);
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
