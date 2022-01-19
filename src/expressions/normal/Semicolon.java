package expressions.normal;

import expressions.special.Expression;

public class Semicolon extends Expression {

	public Semicolon(int line) {
		super(line);
	}
	
	@Override
	public String toString() {
		return "';'";
	}

}
