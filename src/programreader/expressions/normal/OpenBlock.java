package programreader.expressions.normal;

import programreader.expressions.main.CloseBlock;
import programreader.expressions.special.Bracket;
import programreader.expressions.special.Expression;
import programreader.program.ExpressionType;

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
