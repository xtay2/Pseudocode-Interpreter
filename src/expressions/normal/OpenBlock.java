package expressions.normal;

import expressions.main.CloseBlock;
import expressions.special.Bracket;
import expressions.special.Expression;
import parser.program.ExpressionType;

public class OpenBlock extends Expression implements Bracket {

	private CloseBlock myMatch = null;

	public OpenBlock(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.CLOSE_BLOCK);
	}

	@Override
	public void setMyMatch(Bracket match) {
		myMatch = (CloseBlock) match;
	}

	@Override
	public Bracket getMatch() {
		return myMatch;
	}
}
