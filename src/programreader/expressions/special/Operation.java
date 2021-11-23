package programreader.expressions.special;

/**
 * Consist of two ValueHolders and an Operator.
 */
public final class Operation extends Expression implements ValueHolder {

	private Operator operator;
	private ValueHolder a;
	private ValueHolder b;
		
	public Operation(int line, Operator op, ValueHolder a, ValueHolder b) {
		super(line);
		this.operator = op;
		this.a = a;
		this.b = b;
	}
	
	@Override
	public Value getValue() {
		return operator.perform(a, b);
	}

}

