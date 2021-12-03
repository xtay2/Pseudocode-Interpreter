package expressions.main.loops;

import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.ValueHolder;

public class ForEachLoop extends MainExpression implements Scope {

	public ForEachLoop(int line) {
		super(line);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void build(Expression... args) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public int getStart() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getEnd() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getScopeName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isOneLineStatement() {
		// TODO Auto-generated method stub
		return false;
	}

}
