package expressions.normal;

import expressions.special.Expression;

public class Semikolon extends Expression {

	public Semikolon(int line) {
		super(line);
		setExpectedExpressions();
	}
	
	@Override
	public String toString() {
		return "';'";
	}

}
