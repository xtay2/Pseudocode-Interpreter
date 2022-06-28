package interpreting.modules.merger;

import static building.types.abstractions.SuperType.*;
import static building.types.specific.BuilderType.*;
import static building.types.specific.DynamicType.*;
import static building.types.specific.FlagType.*;
import static building.types.specific.KeywordType.*;
import static building.types.specific.datatypes.SingleType.*;
import static interpreting.modules.merger.ValueMerger.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.main.*;
import building.expressions.main.blueprints.*;
import building.expressions.main.blueprints.Module;
import building.expressions.normal.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.types.abstractions.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import building.types.specific.operators.*;
import errorhandeling.*;
import importing.filedata.paths.*;
import interpreting.program.*;
import misc.helper.*;

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
						yield new Literal(lineID, ValueBuilder.stringToLiteral(line.remove(0).value));
					case NAME:
						if (sec != null) {
							if (sec.is(OPEN_BRACKET))
								yield (ValueHolder) buildCall(buildStaticLink());
							if (sec.is(ARRAY_START)) {
								ArrayAccess acc = buildArrayAccess();
								if (!line.isEmpty() && line.get(0).is(ASSIGNMENT_TYPE))
									yield buildAssignment(acc);
								yield acc;
							}
							if (sec.is(ASSIGNMENT_TYPE))
								yield buildAssignment(buildName(VarName.class));
						}
						yield buildName(VarName.class);
					case STATIC_LINK:
						yield null;//
				};
			case BuilderType b:
				yield switch (b) {
					case ARRAY_START:
						yield ValueMerger.buildArrayLiteral();
					case OPEN_BRACKET:
						if (sec != null && sec.is(DATA_TYPE))
							yield buildExplicitCast();
						yield buildBracketedExpression();
					case MULTI_CALL_START:
						yield buildMultiCall();
					// Non-Value-BuilderTypes
					default:
						throw new PseudocodeException("IllegalCodeFormat", "Unexpected symbol: \"" + b + "\".", path);
				};
			case SingleType e:
				yield buildDeclaration();
			case PrefixOpType p:
				yield OpMerger.buildPrefix();
			default:
				throw new AssertionError("Unexpected type \"" + fst.type + "\" at " + path);
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
			default -> throw new AssertionError("Unexpected type " + type + " in " + path);
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
			case IS, IMPORT -> throw new AssertionError("Unexpected type " + type + " in " + path);
		};
	}
	
	/** [Blueprint] [Name] [{] */
	public static Blueprint buildBlueprint() {
		BlueprintType bp = (BlueprintType) line.remove(0).type;
		Name name = buildName(ClassName.class);
		OpenBlock ob = buildOpenBlock();
		return switch (bp) {
			case MODULE -> new Module(lineID, name, ob);
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
	@SuppressWarnings("unchecked")
	protected static <T extends Name> T buildName(Class<T> nameType) {
		Name res = Name.generateName(lineID, line.remove(0).value);
		if (!res.getClass().equals(nameType)) {
			throw new PseudocodeException("InvalidName", //
					"Expected the name for a constant (uppercase letters with underscores) but got \"" + res + "\".", //
					path);
		}
		return (T) res;
	}
	
	/**  */
	protected static Blueprint buildStaticLink() {
		if (line.get(0).is(DynamicType.STATIC_LINK))
			return line.remove(0).value;
		return ((BlueprintPath) path).blueprint;
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
	
	/** [EXPECTED_TYPE] [?] ([ARRAY_START] [RANGE] [ARRAY_END]) */
	protected static DataType buildExpType() {
		SingleType t = (SingleType) line.remove(0).type;
		boolean allowsNull = false;
		if (!line.isEmpty() && line.get(0).is(MAYBE)) {
			line.remove(0);
			allowsNull = true;
		}
		if (line.size() > 1 && line.get(0).is(ARRAY_START)) {
			List<Range> dims = new ArrayList<>(1);
			do {
				line.remove(0);
				dims.add(buildRange());
				line.remove(0);
			} while (line.size() > 1 && line.get(0).is(ARRAY_START));
			return new DataType(t, allowsNull, dims.toArray(new Range[dims.size()]));
		}
		return new DataType(t, allowsNull);
	}
	
	/** [INT?] [..?] [INT?] */
	private static Range buildRange() {
		try {
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
				throw new PseudocodeException("IllegalCodeFormatException",
						"Range-Symbol \"..\" has to be surrounded by atleast one integer-literal.", path);
			}
			return Range.UNBOUNDED;
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, path);
		}
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
				throw new AssertionError("Duplicate flag in " + path);
		}
		Flaggable f = null;
		// Declaration with optional flags
		if (line.get(0).is(DATA_TYPE))
			f = buildDeclaration();
		// Declaration of a constant without type and optional flags
		else if ((flags.contains(CONSTANT) || flags.contains(FINAL)) && line.get(0).is(NAME)) {
			line.add(0, new BuilderExpression(lineID, VAR));
			if (line.size() <= 2 || !line.get(2).is(AssignmentType.NORMAL))
				throw new PseudocodeException("InsufficientDeclaration", "A final variable has to be defined with a value at declaration.",
						path);
			f = buildDeclaration();
		}
		// Definition-Declaration with optional flags
		else if (line.get(0).is(FUNC))
			f = FuncMerger.buildFunc(flags.remove(NATIVE));
		// FlagSpace
		else if (line.get(0).is(OPEN_BLOCK))
			f = StatementMerger.buildFlagSpace();
		else
			throw new AssertionError("Unexpected Type: " + line.get(0).type + " in " + path);
		f.addFlags(flags);
		return f;
	}
}
