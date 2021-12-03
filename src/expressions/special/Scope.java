package expressions.special;

import main.Main;

public interface Scope {

	public static final Scope GLOBAL_SCOPE = new GlobalScope();

	int getStart();

	int getEnd();

	String getScopeName();

	public boolean isOneLineStatement();

}

class GlobalScope implements Scope {

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
	public boolean isOneLineStatement() {
		return false;
	}

}