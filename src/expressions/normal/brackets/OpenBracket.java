package expressions.normal.brackets;

import static parsing.program.ExpressionType.*;

import expressions.normal.Expression;
import expressions.special.Bracket;
import parsing.parser.Call;

/**
 * Used in {@link Call} and {@link BracketedExpression}.
 */
public class OpenBracket extends Expression implements Bracket {

	private CloseBracket match;

	public OpenBracket(int line) {
		super(line);
		setExpectedExpressions(LITERAL, OPEN_BRACKET, CLOSE_BRACKET, EXPECTED_TYPE, NAME, ARRAY_START, CREMENT);
	}

	@Override
	public Bracket getMatch() {
		return match;
	}

	@Override
	public void setMyMatch(Bracket match) {
		throw new AssertionError("This is unimplemented but will be later, when needed.");
	}
}
