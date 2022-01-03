package expressions.normal;

import expressions.special.Bracket;
import expressions.special.Expression;
import helper.Output;
import parser.program.ExpressionType;

public class CloseBracket extends Expression implements Bracket {

	private OpenBracket match;

	public CloseBracket(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.CLOSE_BRACKET, ExpressionType.EXPECTED_RETURN_TYPE,
				ExpressionType.COMMA, ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR, ExpressionType.DEFINITE_LINEBREAK);
	}

	@Override
	public Bracket getMatch() {
		return match;
	}

	@Override
	public void setMyMatch(Bracket match) {
		this.match = (OpenBracket) match;
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "')'";
	}
}
