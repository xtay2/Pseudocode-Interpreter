package interpreting.modules.merger;

import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.operators.PrefixOpType.NOT;

import java.util.ArrayList;
import java.util.List;

import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.IsStatement;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.normal.casting.ExplicitCast;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Literal;
import building.expressions.normal.containers.Name;
import building.expressions.normal.operators.prefix.PrefixOperator;
import building.expressions.possible.Call;
import building.expressions.possible.allocating.Assignment;
import building.expressions.possible.allocating.Declaration;
import building.expressions.possible.multicall.MultiCall;
import building.types.specific.AssignmentType;
import building.types.specific.datatypes.DataType;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;

/** Every merged {@link ValueHolder}. */
public abstract class ValueMerger extends SuperMerger {

	/** [|] [VALUE_HOLDER] [,] [VALUE_HOLDER]... [|] */
	public static MultiCall buildMultiCall() {
		return new MultiCall(lineID, buildParts());
	}

	/** [(] **/
	public static Call buildCall() {
		return new Call(lineID, buildName(), buildParts());
	}

	/* [OPEN_SQUARE] [?PARAM] [?COMMA] [?PARAM] [CLOSE_SQUARE] */
	public static Literal buildArrayLiteral() {
		return new Literal(lineID, buildParts());
	}

	/** [NAME] [ARRAY_START] [VAL_HOLDER] [ARRAY_END] ?([ARRAY_START] [VAL_HOLDER] [ARRAY_END])... */
	public static ArrayAccess buildArrayAccess() {
		Name target = buildName();
		List<ValueHolder> parts = new ArrayList<>();
		while (!line.isEmpty() && line.get(0).is(ARRAY_START)) {
			line.remove(0);// ArrayStart
			parts.add(buildVal()); // INDEX
			line.remove(0);// ArrayEnd
		}
		return new ArrayAccess(lineID, target, parts);
	}

	/** [OPEN_BRACK] [VALUE/OPERATION] [CLOSE_BRACK] */
	public static BracketedExpression buildBracketedExpression() {
		line.remove(0); // OPEN_BRACK
		BracketedExpression b = new BracketedExpression(lineID, buildVal());
		line.remove(0); // CLOSE_BRACK
		return b;
	}

	/** [IS] [?NOT] [EXPECTED_TYPE] */
	public static ValueHolder buildIsStatement(ValueHolder val) {
		line.remove(0); // Is
		if (line.get(0).is(NOT)) {
			line.remove(0); // Not
			return new PrefixOperator(lineID, NOT, new IsStatement(lineID, val, buildExpType()));
		}
		return new IsStatement(lineID, val, buildExpType());
	}

	// ASSIGNING /////////////////////////////////////////////

	/** [NAME] [ASSIGNMENT] [VALUE_HOLDER] */
	public static Assignment buildAssignment(ValueChanger target) {
		AssignmentType type = (AssignmentType) line.remove(0).type; // Assignment
		return new Assignment(lineID, type, target, buildVal());
	}

	/** [EXPECTED_TYPE] [NAME] [ASSIGNMENT] [VALUE_HOLDER] */
	public static Declaration buildDeclaration() {
		DataType type = buildExpType();
		Name name = buildName();
		AssignmentType assignOp = line.isEmpty() ? null : (AssignmentType) line.remove(0).type;
		ValueHolder vH;
		try {
			vH = line.isEmpty() ? type.stdVal() : buildVal();
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, dataPath);
		}
		if (assignOp != null && assignOp != AssignmentType.NORMAL) {
			throw new PseudocodeException("Declaration", //
					"An initial declaration can only utilise the normal assignment operator \"" + AssignmentType.NORMAL + "\".", //
					dataPath);
		}
		return new Declaration(lineID, type, name, vH);
	}

	/** [(] [TYPE] [)] [VALUE_HOLDER] */
	public static ExplicitCast buildExplicitCast() {
		line.remove(0);
		DataType t = buildExpType();
		line.remove(0);
		return new ExplicitCast(lineID, t, buildVal());
	}
}
