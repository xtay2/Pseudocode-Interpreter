package expressions.main.copy;

import expressions.normal.OpenBlock;
import expressions.special.Bracket;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;

public class CloseBlock extends MainExpression implements Bracket {

	private OpenBlock myMatch;

	public CloseBlock(int line) {
		super(line);
		setExpectedExpressions();
	}

	@Override
	public void build(Expression... args) {
		if (args.length > 1)
			throw new IllegalArgumentException("Illegal expression behind bracket: " + args[1]);
	}

	@Override
	public void setMyMatch(Bracket match) {
		myMatch = (OpenBlock) match;
	}

	@Override
	public Bracket getMatch() {
		return myMatch;
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		return true; // Just go back
	}

}
