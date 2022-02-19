package expressions.normal.brackets;

import static types.specific.ExpressionType.CLOSE_SCOPE;
import static types.specific.ExpressionType.OPEN_SCOPE;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ScopeBracket;
import expressions.main.CloseScope;
import modules.parser.program.ProgramLine;

public final class OpenScope extends Expression implements ScopeBracket {

	private CloseScope myMatch;

	public OpenScope(int line) {
		super(line, OPEN_SCOPE, CLOSE_SCOPE);
	}

	@Override
	public ScopeBracket getMatch() {
		if (myMatch == null)
			throw new IllegalCodeFormatException(getOriginalLine(), "Block has no matching end. Use a ; or a }.");
		return myMatch;
	}

	/** Connects with the matching CloseScope in {@link ProgramLine#construct}. */
	public void setMyMatch(CloseScope myMatch) {
		if (this.myMatch != null)
			throw new AssertionError("Match cannot get set twice.");
		this.myMatch = myMatch;
	}
}
