package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.TextValue;

public class ArithmeticOperator extends InfixOperator {

	public ArithmeticOperator(int lineID, InfixOpType op) {
		super(lineID, op);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		return switch (op) {
			case ADD -> add(fst, sec);
			case SUB -> sub(fst, sec);
			case MULT -> mult(fst, sec);
			case DIV -> div(fst, sec);
			case MOD -> mod(fst, sec);
			case POW -> pow(fst, sec);
			case ROOT -> root(fst, sec);
			default -> throw new AssertionError("Unexpected arithmetic operator: " + op);
		};
	}

	@Override
	public ArrayValue executeFor(ValueHolder operand, ValueHolder[] content) {
		Value[] res = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = perform(operand, content[i]);
		return new ArrayValue(res);
	}

	@Override
	public ArrayValue executeFor(ValueHolder[] content, ValueHolder operand) {
		Value[] res = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = perform(content[i], operand);
		return new ArrayValue(res);
	}

	/**
	 * {@link NumberValue#add} numbers, {@link ArrayValue#concat} arrays, or
	 * {@link ArrayValue#append}/{@link ArrayValue#prepend} element to arrays.
	 */
	private Value add(Value a, Value b) {
		// Array Concat
		if (a instanceof ArrayValue a1 && b instanceof ArrayValue a2)
			return a1.concat(a2, getOriginalLine());

		// Arithmetical Addition
		if (a instanceof NumberValue n1 && b instanceof NumberValue n2)
			return n1.add(n2);

		// Array Addition End
		if (a instanceof ArrayValue arr && !(b instanceof ArrayValue))
			return arr.append(b, getOriginalLine());
		// Array Addition Start
		if (!(a instanceof ArrayValue) && b instanceof ArrayValue arr)
			return arr.prepend(a, getOriginalLine());

		return a.asText().concat(b.asText());
	}

	/**
	 * Subtract numbers.
	 *
	 * @see {@link NumberValue#sub}
	 */
	private Value sub(Value a, Value b) {
		return a.asNr().sub(b.asNr());
	}

	private Value mult(Value a, Value b) {
		// Array-Multiplication
		if (a instanceof ArrayValue arr && b instanceof IntValue i)
			return arr.multiply(i.raw().intValueExact(), getOriginalLine());

		// Array-Multiplication
		if (a instanceof IntValue i && b instanceof ArrayValue arr)
			return arr.multiply(i.value.intValueExact(), getOriginalLine());

		// Text-Multiplication
		if (a instanceof TextValue txt && b instanceof IntValue i)
			return txt.multiply(i.value.intValueExact(), getOriginalLine());

		// Text-Multiplication
		if (a instanceof IntValue i && b instanceof TextValue txt)
			return txt.multiply(i.value.intValueExact(), getOriginalLine());

		// Arithmetical Addition
		return a.asNr().mult(b.asNr());
	}

	/**
	 * Divide numbers.
	 *
	 * @see {@link NumberValue#div}
	 */
	private Value div(Value a, Value b) {
		return a.asNr().div(b.asNr());
	}

	/**
	 * Modulate numbers.
	 *
	 * @see {@link NumberValue#mod}
	 */
	private Value mod(Value a, Value b) {
		return a.asNr().mod(b.asNr());
	}

	/**
	 * Potentiate numbers.
	 *
	 * @see {@link NumberValue#pow}
	 */
	private Value pow(Value a, Value b) {
		return a.asNr().pow(b.asNr());
	}

	/**
	 * Extract root from numbers.
	 *
	 * @see {@link NumberValue#root}
	 */
	private Value root(Value a, Value b) {
		return a.asNr().root(b.asNr());
	}
}
