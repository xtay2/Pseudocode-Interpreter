package expressions.normal.brackets;

import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.CLOSE_BRACKET;
import static parsing.program.ExpressionType.CREMENT;
import static parsing.program.ExpressionType.EXPECTED_TYPE;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.special.Bracket;
import expressions.special.Expression;
import helper.Output;

public class OpenBracket extends Expression implements Bracket {

	private CloseBracket match;

	public OpenBracket(int line) {
		super(line);
		setExpectedExpressions(CLOSE_BRACKET, EXPECTED_TYPE, LITERAL, NAME, ARRAY_START, CREMENT);
	}

	@Override
	public Bracket getMatch() {
		return match;
	}

	@Override
	public void setMyMatch(Bracket match) {
		throw new AssertionError("This is unimplemented but can be later, when needed.");
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "'('";
	}
}
