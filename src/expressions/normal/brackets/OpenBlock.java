package expressions.normal.brackets;

import static parsing.program.ExpressionType.CLOSE_BLOCK;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.main.CloseBlock;
import expressions.special.Bracket;
import expressions.special.Expression;
import helper.Output;

public class OpenBlock extends Expression implements Bracket {

	private CloseBlock myMatch = null;

	public OpenBlock(int line) {
		super(line);
		setExpectedExpressions(CLOSE_BLOCK);
	}

	@Override
	public Bracket getMatch() {
		if (myMatch == null)
			throw new IllegalCodeFormatException(getOriginalLine(), "Block has no matching end. Use a ; or a }.");
		return myMatch;
	}

	@Override
	public void setMyMatch(Bracket match) {
		myMatch = (CloseBlock) match;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "'{'";
	}
}
