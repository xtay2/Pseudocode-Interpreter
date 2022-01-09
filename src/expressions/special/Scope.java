package expressions.special;

import expressions.main.CloseBlock;
import expressions.normal.brackets.OpenBlock;
import main.Main;

public abstract class Scope extends MainExpression {

	public static final Scope GLOBAL_SCOPE = new GlobalScope();

	protected OpenBlock block = null;

	public Scope(int line) {
		super(line);
	}

	public int getStart() {
		return line;
	}

	public int getEnd() {
		try {
			return ((CloseBlock) block.getMatch()).line + 1;
		} catch (NullPointerException e) {
			throw new AssertionError("The scope " + this + " doesn't get closed. Use a ; or a }.");
		}
	}

	public abstract String getScopeName();

}

final class GlobalScope extends Scope {

	public GlobalScope() {
		super(0);
	}

	@Override
	public int getStart() {
		return 0;
	}

	@Override
	public int getEnd() {
		return Main.program.size();
	}

	@Override
	public String getScopeName() {
		return "global";
	}

	@Override
	public String toString() {
		return getScopeName();
	}

	@Override
	public void build(Expression... args) throws UnsupportedOperationException {
		throw new UnsupportedOperationException("The global scope should not be treated as an expression.");
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) throws UnsupportedOperationException {
		throw new UnsupportedOperationException("The global scope should not be treated as an expression.");
	}
}