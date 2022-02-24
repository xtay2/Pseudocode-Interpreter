package expressions.normal.operators.infix;

import static types.specific.data.DataType.NUMBER;
import static types.specific.data.DataType.TEXT;

import datatypes.ArrayValue;
import datatypes.Value;
import datatypes.numerical.NumberValue;
import expressions.abstractions.interfaces.ValueHolder;
import types.specific.operators.InfixOpType;

public class ArithmeticOperator extends InfixOperator {

	public ArithmeticOperator(int line, InfixOpType op) {
		super(line, op);
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

	/**
	 * {@link NumberValue#add} numbers, {@link ArrayValue#concat} arrays, or
	 * {@link ArrayValue#append}/{@link ArrayValue#prepend} element to arrays.
	 */
	private Value add(Value a, Value b) {
		// Array Concat
		if (a instanceof ArrayValue a1 && b instanceof ArrayValue a2)
			return a1.concat(a2, getOriginalLine());
		// Arithmetical Addition
		if (a.is(NUMBER) && b.is(NUMBER))
			return a.asNumber().add(b.asNumber());
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
		return a.asNumber().sub(b.asNumber());
	}

	private Value mult(Value a, Value b) {
		// Array-Multiplication
		if (a instanceof ArrayValue arr && b.is(NUMBER))
			return arr.multiply(b.asInt().value.intValueExact(), getOriginalLine());

		// Array-Multiplication
		if (a.is(NUMBER) && b instanceof ArrayValue arr)
			return arr.multiply(a.asInt().value.intValueExact(), getOriginalLine());

		// Arithmetical Addition
		if (a.canCastTo(NUMBER) && b.canCastTo(NUMBER))
			return a.asNumber().mult(b.asNumber());

		// Text-Multiplication
		if (a.is(TEXT) && b.canCastTo(NUMBER))
			return a.asText().multiply(b.asInt().value.intValueExact(), getOriginalLine());

		// Text-Multiplication
		if (a.canCastTo(NUMBER) && b.is(TEXT))
			return b.asText().multiply(a.asInt().value.intValueExact(), getOriginalLine());

		return a.asNumber().mult(b.asNumber());
	}

	/**
	 * Divide numbers.
	 * 
	 * @see {@link NumberValue#div}
	 */
	private Value div(Value a, Value b) {
		return a.asNumber().div(b.asNumber());
	}

	/**
	 * Modulate numbers.
	 * 
	 * @see {@link NumberValue#mod}
	 */
	private Value mod(Value a, Value b) {
		return a.asNumber().mod(b.asNumber());
	}

	/**
	 * Potentiate numbers.
	 * 
	 * @see {@link NumberValue#pow}
	 */
	private Value pow(Value a, Value b) {
		return a.asNumber().pow(b.asNumber());
	}

	/**
	 * Extract root from numbers.
	 * 
	 * @see {@link NumberValue#root}
	 */
	private Value root(Value a, Value b) {
		return a.asNumber().root(b.asNumber());
	}
}
