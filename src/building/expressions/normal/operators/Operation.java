package building.expressions.normal.operators;

import static building.types.abstractions.SpecificType.MERGED;
import static building.types.specific.operators.InfixOpType.AND;
import static building.types.specific.operators.InfixOpType.NAND;
import static building.types.specific.operators.InfixOpType.NOR;
import static building.types.specific.operators.InfixOpType.OR;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.Operatable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.operators.infix.ArithmeticOperator;
import building.expressions.normal.operators.infix.ComparativeOperator;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.infix.LogicalOperator;
import building.expressions.possible.multicall.MultiCall;
import building.expressions.possible.multicall.MultiCallable;
import building.types.specific.operators.InfixOpType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.modules.merger.SuperMerger;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

/**
 * Consist of n Operators and n + 1 ValueHolders.
 */
public final class Operation extends Expression implements MultiCallable, ValueHolder {

	private final List<Operatable> operation;

	/** Gets called when an Operation is constructed in the {@link SuperMerger}. */
	public Operation(int lineID, List<Operatable> op) {
		super(lineID, MERGED);
		if (op.size() < 3)
			throw new AssertionError("An operation has to atleast contain one operator and two values.\nWas " + op);
		this.operation = format(op);
	}

	/** Converts multiple ComparativeOperators */
	private List<Operatable> format(List<Operatable> op) {
		for (int i = 1; i < op.size(); i += 2) {
			if (op.get(i) instanceof ComparativeOperator && i + 2 < op.size() && op.get(i + 2) instanceof ComparativeOperator) {
				op.add(i + 2, new LogicalOperator(lineIdentifier, InfixOpType.AND));
				op.add(i + 3, op.get(i + 1));
				i += 2;
			}
		}
		return Collections.unmodifiableList(op);
	}

	@Override
	public Value getValue() {
		return recValue(new ArrayList<>(operation));
	}

	/**
	 * Recursivly evaluates the result from this {@link Operation}, by performing single operations and
	 * replacing them with their result in the big {@link List}.
	 * 
	 * @param op should be a mutable copy of {@link #operation}.
	 * @return the result of this operation.
	 */
	private Value recValue(List<Operatable> op) {
		ValueHolder a = (ValueHolder) op.get(0);
		InfixOperator o = (InfixOperator) op.get(1);
		if (!(a instanceof MultiCall)) { // Shorten the condition while the first expression is a special case.
			Value special;
			while ((special = isSpecialCase(a.getValue(), o)) != null) {
				do {
					op.remove(2);
					op.remove(1);
					if (op.size() == 1)
						return a.getValue();
				} while (o.op.rank <= ((InfixOperator) op.get(1)).op.rank);
				op.set(0, special);
				a = (ValueHolder) op.get(0);
				o = (InfixOperator) op.get(1);
			}
		}
		if (op.size() == 3)
			return (Value) evaluate(op, 1).get(0);
		if (op.size() > 3) {
			InfixOperator next = (InfixOperator) op.get(3);
			if (next.op.rank > o.op.rank || o.isRightAssociative()) // Evaluate Next
				return recValue(evaluate(op, 3));
			return recValue(evaluate(op, 1)); // Evaluate first
		}
		throw new AssertionError(getOriginalLine() + "Operation-Check failed. Illegal Operation: " + op + ", \noriginally: " + operation);
	}

	/**
	 * Sometimes the outcome of a operation can get evaluated without looking at the whole operation.
	 * 
	 * <pre>
	 * Examples are:
	 * true or b = true
	 * true nor b = false
	 * false nand b = true
	 * false and b = false
	 * 
	 * NaN + b = NaN
	 * </pre>
	 * 
	 * @param a
	 * @param o
	 * @return
	 */
	private Value isSpecialCase(Value a, InfixOperator o) {
		if (a instanceof BoolValue b) {
			if (b.raw()) {
				if (o.is(OR)) // true or b = true
					return BoolValue.TRUE;
				if (o.is(NOR)) // true nor b = false
					return BoolValue.FALSE;
			} else {
				if (o.is(NAND)) // false nand b = true
					return BoolValue.TRUE;
				if (o.is(AND)) // false and b = false
					return BoolValue.FALSE;
			}
		}
		if (a == NAN && o instanceof ArithmeticOperator)
			return NAN; // NaN and every Arithmetic Operator is allways NaN
		return null;
	}

	/**
	 * Evaluates a part of an equation.
	 * 
	 * @param op      is the current state of the whole operation.
	 * @param evalPos should be 1 or 3.
	 * @return the modified whole operation after the small operation got evaluated.
	 */
	private List<Operatable> evaluate(List<Operatable> op, int evalPos) {
		List<Operatable> res = new ArrayList<>(op);
		ValueHolder a = (ValueHolder) op.get(evalPos - 1);
		InfixOperator o = (InfixOperator) op.get(evalPos);
		ValueHolder b = (ValueHolder) op.get(evalPos + 1);
		// Replace operation with result
		res.set(evalPos, perform(a, o, b));
		res.remove(evalPos + 1);
		res.remove(evalPos - 1);
		return res;
	}

	/**
	 * Performs a single infix-operation, that contains two operands (of which one might be a
	 * {@link MultiCall}) and an operator.
	 * 
	 * @return the result of the equation.
	 */
	private Value perform(ValueHolder a, InfixOperator o, ValueHolder b) {
		if (a instanceof MultiCall && b instanceof MultiCall)
			throw new IllegalCodeFormatException(getOriginalLine(), "At most one operand of an infix operation can be a multicall.");
		if (a instanceof MultiCall m)
			return o.executeFor(m.content, b);
		if (b instanceof MultiCall m)
			return o.executeFor(a, m.content);
		return o.perform(a, b);
	}

	@Override
	public String toString() {
		String res = "";
		for (Operatable e : operation) {
			res += switch (e) {
				case InfixOperator io -> " " + io.type + " ";
				case ValueHolder vh -> vh.toString();
				default -> throw new IllegalCodeFormatException(getOriginalLine(), "Illegal Element in Operation: " + e);
			};
		}
		return res;
	}
}
