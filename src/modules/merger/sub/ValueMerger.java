package modules.merger.sub;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.data.ArrayType.VAR_ARRAY;

import java.util.ArrayList;
import java.util.List;

import datatypes.ArrayValue;
import datatypes.DefValue;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.statements.IsStatement;
import expressions.normal.brackets.BracketedExpression;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;
import expressions.possible.Call;
import expressions.possible.assigning.Assignment;
import expressions.possible.assigning.Declaration;
import expressions.possible.multicall.MultiCall;
import modules.merger.SuperMerger;
import types.SuperType;
import types.specific.AssignmentType;
import types.specific.data.ExpectedType;

/**
 * Every merged {@link ValueHolder}.
 */
public abstract class ValueMerger extends SuperMerger {

	/** [|] [VALUE_HOLDER] [,] [VALUE_HOLDER]... [|] */
	public static MultiCall buildMultiCall() {
		return new MultiCall(lineID, null, buildParts());
	}

	/** [(] **/
	public static Call buildCall() {
		DefValue func = new DefValue(buildName().getNameString());
		return new Call(lineID, func, buildParts());
	}

	/* [OPEN_SQUARE] [?PARAM] [?COMMA] [?PARAM] [CLOSE_SQUARE] */
	public static ArrayValue buildArrayLiteral() {
		return new ArrayValue(VAR_ARRAY, buildParts());
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

	/** [IS] [EXPECTED_TYPE] */
	public static IsStatement buildIsStatement(ValueHolder val) {
		line.remove(0); // Is
		return new IsStatement(lineID, val, buildExpType());
	}

	// ASSIGNING /////////////////////////////////////////////

	/** [NAME] [ASSIGNMENT] [VALUE_HOLDER] */
	public static Assignment buildAssignment() {
		Name target = buildName();
		AssignmentType type = (AssignmentType) line.remove(0).type; // Assignment
		return new Assignment(lineID, type, target, buildVal());
	}

	/** [EXPECTED_TYPE] [NAME] [ASSIGNMENT] [VALUE_HOLDER] */
	public static Declaration buildDeclaration() {
		ExpectedType type = (ExpectedType) line.remove(0).type;
		ValueChanger target;
		if (type.is(SuperType.ARRAY_TYPE))
			target = buildArrayAccess();
		else if (type.is(SuperType.DATA_TYPE))
			target = buildName();
		else
			throw new UnexpectedTypeError(orgLine, type);
		line.remove(0); // Assignment
		return new Declaration(lineID, type, target, buildVal());
	}
}
