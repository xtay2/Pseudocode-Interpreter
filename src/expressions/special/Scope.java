package expressions.special;

import expressions.main.CloseBlock;
import expressions.normal.brackets.OpenBlock;
import main.Main;

final class GlobalScope extends Scope {

	public GlobalScope() {
		super(0);
	}

	@Override
	public void build(Expression... args) throws UnsupportedOperationException {
		throw new UnsupportedOperationException("The global scope should not be treated as an expression.");
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) throws UnsupportedOperationException {
		throw new UnsupportedOperationException("The global scope should not be treated as an expression.");
	}

	@Override
	public int getEnd() {
		return Main.PROGRAM.size();
	}

	@Override
	public String getScopeName() {
		return "global";
	}

	@Override
	public int getStart() {
		return 0;
	}

	@Override
	public String toString() {
		return getScopeName();
	}
}

public abstract class Scope extends MainExpression {

	public static final Scope GLOBAL_SCOPE = new GlobalScope();

	protected OpenBlock block = null;

	public Scope(int line) {
		super(line);
	}

	public int getEnd() {
		try {
			return ((CloseBlock) block.getMatch()).lineIdentifier + 1;
		} catch (NullPointerException e) {
			throw new AssertionError("The scope " + this + " doesn't get closed. Use a ; or a }.");
		}
	}

	public abstract String getScopeName();

	public int getStart() {
		return lineIdentifier;
	}

}