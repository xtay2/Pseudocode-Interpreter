package expressions.normal.brackets;

import expressions.special.Bracket;
import expressions.special.Expression;
import helper.Output;
import parser.program.ExpressionType;

public class OpenBracket extends Expression implements Bracket {

	private CloseBracket match;

	public OpenBracket(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.CLOSE_BRACKET, ExpressionType.EXPECTED_TYPE, ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	@Override
	public Bracket getMatch() {
		return match;
	}

	@Override
	public void setMyMatch(Bracket match) {
		//TODO : Implement me!
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "'('";
	}
}
