package parser.program;

import java.util.ArrayList;

import expressions.main.Call;
import expressions.main.functions.Function;
import expressions.normal.CloseBracket;
import expressions.normal.Comma;
import expressions.normal.Literal;
import expressions.normal.Name;
import expressions.normal.OpenBracket;
import expressions.normal.operators.Operation;
import expressions.normal.operators.Operator;
import expressions.special.Expression;
import expressions.special.ValueHolder;

public final class ValueBuilder {

	private static ArrayList<Expression> list;

	private static int lineIndex;

	public static ArrayList<Expression> buildLine(ArrayList<Expression> l, int line) {
		list = l;
		lineIndex = line;
		for (int i = 0; i < list.size(); i++)
			if (list.get(i) instanceof Literal || list.get(i) instanceof Name)
				build(i, false);
		return list;
	}

	/**
	 * Recursivly builds an expression.
	 *
	 * @param i                 is the index in list.
	 * @param buildingOperation default is false.
	 */
	private static ValueHolder build(int i, boolean buildingOperation) {
		if (list.get(i) instanceof ValueHolder) {
			if (list.get(i) instanceof Literal) {
				// LITERAL with following OPERATOR
				if (i + 1 < list.size() && list.get(i + 1) instanceof Operator) {
					Operation op = new Operation(i, buildOperation(i));
					list.add(i, op);
					return op;
				}
				// LITERAL without following OPERATOR
				else
					return (Literal) list.get(i);
			}
			if (i + 1 < list.size() && list.get(i) instanceof Name)
				// NAME without following OPERATOR
				if (list.get(i + 1) instanceof Comma || list.get(i + 1) instanceof CloseBracket)
					return (Name) list.get(i);
				else if (list.get(i + 1) instanceof Operator) {
					Operation op = new Operation(i, buildOperation(i));
					list.add(i, op);
					return op;
				}
				// FUNCTION_CALL
				else if (list.get(i + 1) instanceof OpenBracket && !(list.get(0) instanceof Function)) {
					Call c = new Call(lineIndex);
					c.init(((Name) list.get(i)).getName(), buildParams(i + 2));
					list.set(i, c); // Name
					list.remove(i + 1);
					// OPERATOR nach CALL
					if (i + 1 < list.size() && list.get(i + 1) instanceof Operator && !buildingOperation) {
						Operation op = new Operation(lineIndex, buildOperation(i));
						list.add(i, op);
						return op;
					}
					return c;
				}
		}
		return null;
	}

	private static ArrayList<ValueHolder> buildParams(int pos) {
		ArrayList<ValueHolder> params = new ArrayList<>();
		for (int i = pos; i < list.size(); i++) {
			if (list.get(i) instanceof CloseBracket)
				break;
			if (list.get(i) instanceof Name || list.get(i) instanceof Literal) {
				ValueHolder p = build(i, false);
				params.add(p);
			}
		}
		while (!(list.get(pos) instanceof CloseBracket))
			list.remove(pos);
		list.remove(pos);
		return params;
	}

	private static ArrayList<Expression> buildOperation(int pos) {
		ArrayList<Expression> operation = new ArrayList<>();
		for (int i = pos; i < list.size(); i++)
			if (list.get(i) instanceof Literal || list.get(i) instanceof Call || list.get(i) instanceof Operator)
				operation.add(list.get(i));
			else if (list.get(i) instanceof Name) {
				if (i + 1 < list.size() && list.get(i + 1) instanceof OpenBracket)
					operation.add((Expression) build(i, true));
				else
					operation.add(list.get(i));
			} else
				break;
		for (int i = 0; i < operation.size(); i++)
			list.remove(pos);
		return operation;
	}
}