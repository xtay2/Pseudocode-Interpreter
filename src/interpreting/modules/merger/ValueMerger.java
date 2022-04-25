package interpreting.modules.merger;

import static building.types.specific.BuilderType.ARRAY_END;
import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.BuilderType.RANGE;
import static building.types.specific.DynamicType.LITERAL;
import static building.types.specific.datatypes.ArrayType.*;
import static building.types.specific.operators.PrefixOpType.NOT;

import java.util.ArrayList;
import java.util.List;

import building.expressions.abstractions.Range;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.IsStatement;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.normal.casting.ExplicitCast;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.expressions.normal.operators.prefix.PrefixOperator;
import building.expressions.possible.Call;
import building.expressions.possible.allocating.Assignment;
import building.expressions.possible.allocating.Declaration;
import building.expressions.possible.multicall.MultiCall;
import building.types.specific.AssignmentType;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import interpreting.exceptions.IllegalCodeFormatException;
import misc.helper.MathHelper;
import runtime.datatypes.array.ArrayValue;

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
	public static ArrayValue buildArrayLiteral() {
		return new ArrayValue(VAR_ARRAY, true, buildParts());
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
		DataType type = (SingleType) line.remove(0).type;
		boolean allowsNull = checkIfNullAllowed();
		type = buildArrayDimensions((SingleType) type);
		Name name = buildName();
		AssignmentType assignOp = line.isEmpty() ? null : (AssignmentType) line.remove(0).type;
		ValueHolder vH = line.isEmpty() ? type.stdVal(allowsNull) : buildVal();
		if (assignOp != null && assignOp != AssignmentType.NORMAL) {
			throw new IllegalCodeFormatException(orgLine,
					"An initial declaration can only utilise the normal assignment operator \"" + AssignmentType.NORMAL + "\".");
		}
		return new Declaration(lineID, type, allowsNull, name, vH);
	}

	/** [(INT?)] ([(INT?)]?) ([(INT?)]?)... */
	private static DataType buildArrayDimensions(SingleType type) {
		if (line.size() > 1 && line.get(0).is(ARRAY_START)) {
			List<Range> dims = new ArrayList<>(1);
			do {
				line.remove(0);
				dims.add(buildRange());
				line.remove(0);
			} while (line.size() > 1 && line.get(0).is(ARRAY_START));
			return new ArrayType(type, dims.toArray(new Range[dims.size()]));
		}
		return type;
	}

	/** [INT?] [..?] [INT?] */
	private static Range buildRange() {
		if (line.get(0).is(LITERAL)) {
			int lower = MathHelper.valToInt(buildVal());
			if (line.get(0).is(RANGE)) {
				line.remove(0);
				if (line.get(0).is(LITERAL))
					return Range.intervalBound(lower, MathHelper.valToInt(buildVal()));
				return Range.lowerBound(lower);
			}
			return Range.exact(lower);
		} else if (line.get(0).is(RANGE)) {
			line.remove(0);
			if (line.get(0).is(LITERAL))
				return Range.upperBound(MathHelper.valToInt(buildVal()));
			throw new IllegalCodeFormatException(orgLine, "Range-Symbol \"..\" has to be surrounded by atleast one integer-literal.");
		}
		return Range.UNBOUNDED;
	}

	/** [(] [TYPE] [)] [VALUE_HOLDER] */
	public static ExplicitCast buildExplicitCast() {
		line.remove(0);
		DataType t = buildExpType();
		if (line.remove(0).is(ARRAY_START) && line.remove(0).is(ARRAY_END)) {
			t = switch ((SingleType) t) {
				case VAR -> VAR_ARRAY;
				case BOOL -> BOOL_ARRAY;
				case CHAR -> CHAR_ARRAY;
				case INT -> INT_ARRAY;
				case NUMBER -> NUMBER_ARRAY;
				case TEXT -> TEXT_ARRAY;
				case OBJECT -> OBJECT_ARRAY;
			};
			line.remove(0);
		} // For else: Close bracket gets removed in if.
		return new ExplicitCast(lineID, t, buildVal());
	}
}
