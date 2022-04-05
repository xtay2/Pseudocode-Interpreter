package interpreting.modules.merger;

import static building.types.abstractions.SuperType.*;
import static building.types.specific.BuilderType.*;
import static building.types.specific.DataType.VAR;
import static building.types.specific.DynamicType.NAME;
import static building.types.specific.FlagType.CONSTANT;
import static building.types.specific.FlagType.FINAL;
import static building.types.specific.FlagType.NATIVE;
import static building.types.specific.KeywordType.FUNC;
import static building.types.specific.KeywordType.IS;
import static interpreting.modules.merger.ValueMerger.buildAssignment;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.CloseBlock;
import building.expressions.normal.BuilderExpression;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.types.abstractions.AbstractType;
import building.types.specific.AssignmentType;
import building.types.specific.BuilderType;
import building.types.specific.DataType;
import building.types.specific.DynamicType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.operators.PrefixOpType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.program.ValueBuilder;
import runtime.exceptions.UnexpectedTypeError;

/**
 * Every build-Method should atleast remove the first element of the line.
 * 
 * The subclasses of this are all indirectly called by the switch-cases in {@link #build()}.
 */
public abstract class SuperMerger extends ExpressionMerger {

	/**
	 * The default implementation for {@link #buildVal(boolean inOperation)} if no operation gets
	 * evaluated.)
	 */
	protected static ValueHolder buildVal() {
		return buildVal(false);
	}

	/**
	 * Constructs an {@link ValueHolder} from the {@link AbstractType} of the first
	 * {@link BuilderExpression}.
	 */
	protected static ValueHolder buildVal(boolean inOperation) {
		BuilderExpression fst = line.get(0);
		BuilderExpression sec = line.size() > 1 ? line.get(1) : null;
		ValueHolder result = switch (fst.type) {
			case DynamicType e:
				yield switch (e) {
					case LITERAL:
						yield ValueBuilder.stringToLiteral(line.remove(0).value);
					case NAME:
						if (sec != null) {
							if (sec.is(OPEN_BRACKET))
								yield ValueMerger.buildCall();
							if (sec.is(ARRAY_START)) {
								ArrayAccess acc = ValueMerger.buildArrayAccess();
								if (!line.isEmpty() && line.get(0).is(ASSIGNMENT_TYPE))
									yield buildAssignment(acc);
								yield acc;
							}
							if (sec.is(ASSIGNMENT_TYPE))
								yield buildAssignment(buildName());
						}
						yield buildName();
				};
			case BuilderType b:
				yield switch (b) {
					case ARRAY_START:
						yield ValueMerger.buildArrayLiteral();
					case OPEN_BRACKET:
						if (sec != null && sec.is(DATA_TYPE))
							yield ValueMerger.buildExplicitCast();
						yield ValueMerger.buildBracketedExpression();
					case MULTI_CALL_START:
						yield ValueMerger.buildMultiCall();
					// Non-Value-BuilderTypes
					default:
						throw new IllegalCodeFormatException(orgLine, "Unexpected symbol: \"" + b + "\".");
				};
			case DataType e:
				yield ValueMerger.buildDeclaration();
			case PrefixOpType p:
				yield OpMerger.buildPrefix();
			default:
				throw new UnexpectedTypeError(orgLine, fst.type);
		};
		// Check for follow-ups.
		if (!line.isEmpty()) {
			if (line.get(0).is(POSTFIX_OP_TYPE))
				result = OpMerger.buildPostfix(result);
			if (!line.isEmpty()) { // Second check, if first if comes through
				if (!inOperation && line.get(0).is(INFIX_OP_TYPE))
					result = OpMerger.buildOperation(result);
				else if (line.get(0).is(IS))
					result = ValueMerger.buildIsStatement(result);
			}
		}
		return result;
	}

	/**
	 * Constructs an {@link Expression} from a {@link BuilderType}. This includes Scopes.
	 */
	protected static Expression buildAbstract() {
		BuilderType type = (BuilderType) line.get(0).type;
		return switch (type) {
			// Simple BuilderTypes
			case OPEN_BLOCK -> buildOpenBlock();
			case CLOSE_BLOCK -> buildCloseBlock();
			// Complex BuilderTypes
			case ARRAY_START, OPEN_BRACKET, MULTI_CALL_START -> (Expression) buildVal();
			// Decorative BuilderTypes
			default -> throw new UnexpectedTypeError(orgLine, type);
		};
	}

	/** Constructs an {@link Expression} from a {@link KeywordType}. */
	protected static Expression buildKeyword() {
		KeywordType type = (KeywordType) line.get(0).type;
		return switch (type) {
			// Loops
			case FOR -> LoopMerger.buildForEach();
			case REPEAT -> LoopMerger.buildRepeat();
			case FROM -> LoopMerger.buildFromTo();
			case WHILE, UNTIL -> LoopMerger.buildConditional(type);
			// Statements
			case IF, ELIF, ANY, ELSE -> StatementMerger.buildConditional(type);
			case RETURN -> StatementMerger.buildReturn();
			// Callables
			case FUNC -> FuncMerger.buildFunc(false);
			case MAIN -> FuncMerger.buildMain();
			case IS, IMPORT -> throw new UnexpectedTypeError(orgLine, type);
		};
	}

	/** [{] */
	protected static OpenBlock buildOpenBlock() {
		line.remove(0);
		return new OpenBlock(lineID);
	}

	/** [}] */
	protected static CloseBlock buildCloseBlock() {
		line.remove(0);
		return new CloseBlock(lineID);
	}

	/** [NAME] **/
	protected static Name buildName() {
		return new Name(lineID, line.remove(0).value);
	}

	/** [Start] [ValueHolder] ((,) [ValueHolder])] [End] */
	protected static ValueHolder[] buildParts() {
		List<ValueHolder> parts = new ArrayList<>();
		line.remove(0); // Start
		do {
			Expression fst = line.get(0);
			if (!fst.is(CLOSE_BRACKET) && !fst.is(ARRAY_END) && !fst.is(MULTI_CALL_END))
				parts.add(buildVal());
		} while (line.remove(0).is(COMMA));
		return parts.toArray(new ValueHolder[parts.size()]);
	}

	/** [EXPECTED_TYPE] */
	protected static DataType buildExpType() {
		return (DataType) line.remove(0).type;
	}

	/**
	 * Super-Routine for all Flaggables.
	 * 
	 * @param flags are the flags. They get set by this Method.
	 */
	protected static Flaggable buildFlaggable() {
		Set<FlagType> flags = new HashSet<>();
		while (line.get(0).is(FLAG_TYPE)) {
			if (!flags.add((FlagType) line.remove(0).type))
				throw new IllegalCodeFormatException(orgLine, "Duplicate flag. Line: " + orgLine);
		}
		Flaggable f = null;
		// Declaration with optional flags
		if (line.get(0).type instanceof DataType)
			f = ValueMerger.buildDeclaration();
		// Declaration of a constant without type and optional flags
		else if ((flags.contains(CONSTANT) || flags.contains(FINAL)) && line.get(0).is(NAME)) {
			line.add(0, new BuilderExpression(lineID, VAR));
			if (line.size() <= 2 || !line.get(2).is(AssignmentType.NORMAL))
				throw new IllegalCodeFormatException(orgLine, "A final variable has to be defined with a value at declaration.");
			f = ValueMerger.buildDeclaration();
		}
		// Definition-Declaration with optional flags
		else if (line.get(0).is(FUNC))
			f = FuncMerger.buildFunc(flags.remove(NATIVE));
		// FlagSpace
		else if (line.get(0).is(OPEN_BLOCK))
			f = StatementMerger.buildFlagSpace();
		else
			throw new UnexpectedTypeError(line.get(0).type);
		f.addFlags(flags);
		return f;
	}
}
