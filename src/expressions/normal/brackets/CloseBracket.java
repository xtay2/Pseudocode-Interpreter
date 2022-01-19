package expressions.normal.brackets;

import static parsing.program.ExpressionType.ARRAY_END;
import static parsing.program.ExpressionType.CLOSE_BRACKET;
import static parsing.program.ExpressionType.COMMA;
import static parsing.program.ExpressionType.DEFINITE_LINEBREAK;
import static parsing.program.ExpressionType.EXPECTED_RETURN_TYPE;
import static parsing.program.ExpressionType.INFIX_OPERATOR;
import static parsing.program.ExpressionType.OPEN_BLOCK;

import expressions.special.Bracket;
import expressions.special.Expression;
import helper.Output;

public class CloseBracket extends Expression implements Bracket {

	private OpenBracket match;

	public CloseBracket(int line) {
		super(line);
		setExpectedExpressions(CLOSE_BRACKET, EXPECTED_RETURN_TYPE, COMMA, OPEN_BLOCK, INFIX_OPERATOR, ARRAY_END, DEFINITE_LINEBREAK);
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
