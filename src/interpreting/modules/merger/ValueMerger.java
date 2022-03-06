package interpreting.modules.merger;

import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.data.ArrayType.VAR_ARRAY;

import java.util.ArrayList;
import java.util.List;

import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.IsStatement;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.expressions.possible.Call;
import building.expressions.possible.allocating.Assignment;
import building.expressions.possible.allocating.Declaration;
import building.expressions.possible.multicall.MultiCall;
import building.types.SuperType;
import building.types.specific.AssignmentType;
import building.types.specific.data.ExpectedType;
import runtime.datatypes.DefValue;
import runtime.datatypes.array.UnitialisedArrayValue;
import runtime.exceptions.UnexpectedTypeError;

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
	public static UnitialisedArrayValue buildArrayLiteral() {
		return new UnitialisedArrayValue(VAR_ARRAY, buildParts());
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
		if (line.size() > 0 && line.get(0).is(AssignmentType.NORMAL)) {
			line.remove(0); // Assignment
			return new Declaration(lineID, type, target, buildVal());
		}
		return new Declaration(lineID, type, target, type.stdVal());
	}
}
