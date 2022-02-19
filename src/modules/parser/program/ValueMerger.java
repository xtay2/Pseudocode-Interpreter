package modules.parser.program;

import static types.SuperType.*;
import static types.specific.BuilderType.*;
import static types.specific.FlagType.*;
import static types.specific.KeywordType.*;
import static types.specific.ExpressionType.*;
import static types.specific.DataType.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import datatypes.ArrayValue;
import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.CloseScope;
import expressions.main.Declaration;
import expressions.main.OperationAssignment;
import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.functions.NativeFunction;
import expressions.main.functions.Returnable;
import expressions.main.loops.ConditionalLoop;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.loops.Loop;
import expressions.main.loops.RepeatLoop;
import expressions.main.statements.ConditionalStatement;
import expressions.main.statements.IsStatement;
import expressions.main.statements.ReturnStatement;
import expressions.main.statements.Statement;
import expressions.normal.BuilderExpression;
import expressions.normal.ExpectedType;
import expressions.normal.brackets.BracketedExpression;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;
import expressions.normal.flag.Flaggable;
import expressions.normal.operators.Operation;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import expressions.possible.Assignment;
import expressions.possible.Call;
import expressions.possible.Crement;
import types.specific.DataType;
import types.specific.ExpressionType;
import types.specific.FlagType;

/**
 * The new and better ValueBuilder.
 */
public abstract class ValueMerger {

	static List<Expression> line;

	// Only used for debugging.
	static List<Expression> orgLine;

	static int lineID;
	static int lineIndex;

	/**
	 * Takes all pure {@link Expression}s from a {@link ProgramLine} as input and merges them into a
	 * {@link MainExpression}.
	 * 
	 * @param line
	 */
	static MainExpression buildLine(List<Expression> myLine, final int myLineID, final int myLineIndex) {
		line = new ArrayList<>(myLine);
		orgLine = Collections.unmodifiableList(new ArrayList<>(line));
		lineID = myLineID;
		lineIndex = myLineIndex;
		try {
			MainExpression main = (MainExpression) build();
			if (main == null || !line.isEmpty())
				throw new AssertionError(
						"Main-Merge got finished too early or was null.\nMain: " + main + "\nOriginal Line:" + orgLine + "\nLine: " + line);
			return main;
		} catch (ClassCastException | IndexOutOfBoundsException e) {
			e.printStackTrace();
			System.err.print("\nCaused: ");
			throw new IllegalCodeFormatException(myLineIndex,
					"Unknown unpropper format." + "\nOriginal state of line " + orgLine + "\nCurrent state of line: " + line);
		} catch (NullPointerException e) {
			System.out.println(orgLine + "\n" + line);
			e.printStackTrace();
			throw new AssertionError("That shouldn't happen.");
		}
	}

	/**
	 * Call this if the Merger is not currently evaluating an {@link Operation}.
	 * (Default-Implementation)
	 */
	private static Expression build() {
		return build(false);
	}

	/**
	 * Takes the first identifieable group, and executes the according build-sub-routine.
	 * 
	 * @param isInOperation is true if the call to this build-function was made by
	 *                      {@link ValueMerger#buildOperation()} and no new recursive calls of
	 *                      buildOperation are allowed. If this should be false, call {@link #build()}
	 *                      instead.
	 */
	private static Expression build(boolean isInOperation) {
		Expression fst = line.get(0);
		Expression sec = line.size() > 1 ? line.get(1) : null;
		// Build the right MainExpression through recursive pattern matching.
		Expression result = (Expression) switch (fst) {
			case Name name:
				if (sec == null)
					yield line.remove(0);
				else if (sec.is(OPEN_BRACKET))
					yield buildCall();
				else if (sec.is(ARRAY_START))
					yield buildArrayAccess();
				else
					yield switch (sec) {
						case Crement crement -> buildPostCrement();
						case Assignment assignment -> buildAssignment();
						case OperationAssignment opAssign -> buildOperationAssignment();
						case IsStatement is -> buildIsStatement();
						case Operator operation -> isInOperation ? line.remove(0) : buildOperation();
						default -> line.remove(0);
					};
			case Value value:
				yield switch (sec) {
					case IsStatement is -> buildIsStatement();
					case Operator operation -> isInOperation ? line.remove(0) : buildOperation();
					case null -> line.remove(0);
					default -> line.remove(0);
				};
			case ArrayAccess access:
				yield switch (sec) {
					case IsStatement is -> buildIsStatement();
					case Operator operation -> isInOperation ? line.remove(0) : buildOperation();
					case null -> line.remove(0);
					default -> line.remove(0);
				};
			case Loop loop:
				yield switch (loop) {
					case ForEachLoop forEach -> buildForEach();
					case RepeatLoop repeat -> buildRepeat();
					case FromToLoop fromTo -> buildFromTo();
					case ConditionalLoop whileUntil -> buildWhileUntil();
					default -> throw new AssertionError("Undefined Loop: " + loop);
				};
			case Statement statement:
				yield switch (statement) {
					case ConditionalStatement elif -> buildElif();
					case ReturnStatement returnStmt -> buildReturn();
					default -> throw new AssertionError("Undefined Statement: " + statement);
				};
			case CloseScope closeScope:
				yield (CloseScope) line.remove(0);
			case ExpectedType type:
				yield buildDeclaration();
			case Crement crement:
				yield buildPreCrement();
			case MainFunction main:
				yield buildMain();
			case Function func:
				yield buildFunc();
			case BuilderExpression build: {
				if (build.is(ARRAY_START))
					yield buildArrayLiteral();
				if (build.is(OPEN_BRACKET))
					yield buildBracketedExpression();
				if (build.is(FLAG_TYPE)) {
					Set<FlagType> flags = new HashSet<>();
					while (line.get(0).is(FLAG_TYPE)) {
						if (!flags.add((FlagType) line.remove(0).type))
							throw new IllegalCodeFormatException(lineIndex, "Duplicate flag. Line: " + orgLine);
					}

					yield buildFlaggable(flags);
				}

			}
			default:
				throw new AssertionError("Unexpected token \"" + fst + "\" in line " + lineIndex + ".");
		};
		// Wenn gebauter ValueHolder muss nach Operatorenverknüpfung getestet werden.
		if (!line.isEmpty() && !isInOperation && line.get(0) instanceof Operator) {
			line.add(0, result);
			return buildOperation();
		}
		if (!line.isEmpty() && line.get(0) instanceof Assignment) {
			line.add(0, result);
			return buildAssignment();
		}
		return result;
	}

	// BUILD-SUB-ROUTINES ----------------------------------------------------------

	/**
	 * Builds a PreCrement from the first two Expressions in pure.
	 * 
	 * [CREMENT] [NAME]
	 */
	private static Crement buildPreCrement() {
		Crement c = (Crement) line.remove(0);
		c.merge(c, build());
		return c;
	}

	/**
	 * Builds a PostCrement from the first two Expressions in pure.
	 * 
	 * [NAME] [CREMENT]
	 */
	private static Crement buildPostCrement() {
		Crement c = (Crement) line.remove(1);
		c.merge(build(), c);
		return c;
	}

	/**
	 * Builds an Assignment from three Expressions.
	 * 
	 * [NAME] [=] [VALUE]
	 */
	private static Assignment buildAssignment() {
		Assignment a = (Assignment) line.remove(1); // Remove Assign-Operator
		a.merge(line.remove(0), build()); // Name, Value
		return a;
	}

	/**
	 * Builds an Operation as long as every other Expression is an {@link Operator}.
	 * 
	 * [ValueHolder] ([Operator] [ValueHolder])...
	 */
	private static Operation buildOperation() {
		Operation op = new Operation(lineID);
		List<Expression> parts = new ArrayList<>();
		parts.add(line.remove(0));
		while (!line.isEmpty() && line.get(0) instanceof Operator) {
			parts.add(line.remove(0));
			parts.add(build(true));
		}
		op.merge(parts);
		return op;
	}

	/**
	 * Builds a call and packages its parameters.
	 * 
	 * <pre>
	 * [NAME] [(] ([?PARAM] [?COMMA])... [)]
	 * </pre>
	 */
	private static Call buildCall() {
		Call c = new Call(lineID);
		List<Expression> parts = new ArrayList<>();
		parts.add(line.remove(0)); // Name
		line.remove(0); // Remove OpenBracket
		do {
			if (!(line.get(0).is(CLOSE_BRACKET)))
				parts.add(build());
		} while (line.remove(0).is(COMMA));
		c.merge(parts);
		return c;
	}

	/**
	 * Builds an ArrayLiteral and packages its parameters.
	 * 
	 * <pre>
	 * [OPEN_SQUARE] [?PARAM] [?COMMA] [?PARAM] [CLOSE_SQUARE]
	 * </pre>
	 */
	private static ArrayValue buildArrayLiteral() {
		ArrayValue e = new ArrayValue(DataType.VAR_ARRAY);
		List<Expression> parts = new ArrayList<>();
		line.remove(0); // Remove OpenBrack
		do {
			if (!(line.get(0).is(ARRAY_END)))
				parts.add(build());
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		// Filter out Commas
		e.merge(parts);
		return e;
	}

	/**
	 * Builds an ArrayAccess and packages its indices.
	 * 
	 * <pre>
	 * [NAME] [ARRAY_START] [VAL_HOLDER] [ARRAY_END] ?([ARRAY_START] [VAL_HOLDER] [ARRAY_END])...
	 * </pre>
	 */
	private static ArrayAccess buildArrayAccess() {
		ArrayAccess a = new ArrayAccess(lineID);
		List<Expression> parts = new ArrayList<>();
		parts.add(line.remove(0)); // NAME
		while (!line.isEmpty() && line.get(0).is(ARRAY_START)) {
			line.remove(0);// ArrayStart
			parts.add(build()); // INDEX
			line.remove(0);// ArrayEnd
		}
		a.merge(parts);
		return a;
	}

	/**
	 * [VALUE_HOLDER] [IS] [EXPECTED_TYPE]
	 */
	private static IsStatement buildIsStatement() {
		IsStatement e = (IsStatement) line.remove(1);
		e.merge(build(), line.remove(0));
		return e;
	}

	/**
	 * Boxes a Value/Operation into a BracketedExpression.
	 * 
	 * <pre>
	 * [OPEN_BRACK] [VALUE/OPERATION] [CLOSE_BRACK]
	 * </pre>
	 */
	private static BracketedExpression buildBracketedExpression() {
		BracketedExpression b = new BracketedExpression(lineID);
		line.remove(0); // OPEN_BRACK
		b.merge(build());
		line.remove(0); // CLOSE_BRACK
		return b;
	}

	/** [IF/ELIF/ELSE] [?BOOL] [OPEN_SCOPE] */
	private static ConditionalStatement buildElif() {
		ConditionalStatement e = (ConditionalStatement) line.remove(0);
		if (e.is(ELSE))
			e.merge(line.remove(0)); // OpenScope
		else // e is instance of If or Elif
			e.merge(build(), line.remove(0)); // BoolExp, OpenScope
		return e;
	}

	/** [FOR] [NAME] [IN] [CONTAINER] [OPEN_SCOPE] */
	private static ForEachLoop buildForEach() {
		ForEachLoop e = new ForEachLoop(lineID);
		line.remove(0); // For-Keyword
		if (!(line.remove(1) instanceof Operator o && o.op == InfixOperator.IN))
			throw new IllegalCodeFormatException(lineIndex, "The For-Each-Loop has to contain the \"in\"-Keyword.");
		e.merge(line.remove(0), build(), line.remove(0)); // Name, Container, OpenScope
		return e;
	}

	/** [REPEAT] [REPETITIONS] [OPEN_SCOPE] */
	private static RepeatLoop buildRepeat() {
		RepeatLoop e = new RepeatLoop(lineID);
		line.remove(0); // Repeat-Keyword
		e.merge(line.get(0) instanceof OpenScope ? null : build(), line.remove(0)); // Repetitions, OpenScope
		return e;
	}

	/** [WHILE/UNTIL] [CONDITION] [OPEN_SCOPE] */
	private static ConditionalLoop buildWhileUntil() {
		ConditionalLoop e = (ConditionalLoop) line.remove(0);
		e.merge(build(), line.remove(0));
		return e;
	}

	/** [FROM] [NUMBER] [TO] [NUMBER] (?[|] [INTERVALL]) */
	private static FromToLoop buildFromTo() {
		FromToLoop e = (FromToLoop) line.remove(0);
		ValueHolder from = (ValueHolder) build();
		if (!line.remove(0).is(TO))// To-Keyword
			throw new IllegalCodeFormatException(lineIndex, "Missing \"to\"-Keyword in from-to-loop.");
		ValueHolder to = (ValueHolder) build();
		if (line.get(0).is(STEP)) {
			line.remove(0); // LoopConnector
			e.merge((Expression) from, (Expression) to, build(), line.remove(0));
		} else
			e.merge((Expression) from, (Expression) to, null, line.remove(0));
		return e;
	}

	/** [RETURN] [VALUE] */
	private static ReturnStatement buildReturn() {
		ReturnStatement e = (ReturnStatement) line.remove(0);
		e.merge(build());
		return e;
	}

	/** [MAIN] [OPEN_SCOPE] */
	private static MainFunction buildMain() {
		MainFunction e = (MainFunction) line.remove(0);
		e.merge(line.remove(0)); // OpenScope
		return e;
	}

	/** [NAME] [OP_ASSIGN] [VALUE_HOLDER] */
	private static OperationAssignment buildOperationAssignment() {
		OperationAssignment e = (OperationAssignment) line.remove(1);
		e.merge(line.remove(0), line.remove(0));
		return e;
	}

	/**
	 * Super-Routine for all Flaggables.
	 * 
	 * @param flags are the flags. They get set by this Method.
	 */
	private static Flaggable buildFlaggable(Set<FlagType> flags) {
		Flaggable f = null;
		// Declaration with optional flags
		if (line.get(0) instanceof ExpectedType)
			f = buildDeclaration();
		// Declaration of a constant without type and optional flags
		else if (flags.contains(CONSTANT) && line.get(0).is(NAME)) {
			line.add(0, new ExpectedType(VAR, lineID));
			f = buildDeclaration();
		}
		// Returnable-Declaration with optional flags
		else if (line.get(0) instanceof Returnable) {
			if (flags.remove(FlagType.NATIVE))
				f = buildNativeFunc();
			else
				f = buildFunc();
		} else
			throw new IllegalArgumentException("Unknown Flaggable." + line.get(0));
		f.setFlags(flags);
		return f;
	}

	/** [EXPECTED_TYPE] [NAME] [ASSIGNMENT] [VALUE_HOLDER] */
	private static Declaration buildDeclaration() {
		DataType type = ((ExpectedType) line.remove(0)).type; // Remove Type
		Name name = (Name) line.remove(0); // Remove Name
		line.remove(0); // Remove Assignment
		Declaration e = new Declaration(lineID, type);
		e.merge(name, build()); // Remove Value
		return e;
	}

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	private static Function buildFunc() {
		// FUNC
		Function e = (Function) line.remove(0);
		List<Expression> params = new ArrayList<>();
		params.add((Name) line.remove(0)); // Save Name
		line.remove(0); // OpenBrack
		// PARAMETERS
		do {
			if (line.get(0) instanceof ExpectedType t)
				params.add((ExpectedType) line.remove(0));
			if (line.get(0) instanceof Name)
				params.add((Name) line.remove(0));
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		// RETURN_TYPE
		if (!line.isEmpty() && line.get(0).is(EXPECTED_RETURN_TYPE)) {
			line.remove(0); // Pfeilsymbol
			params.add((ExpectedType) line.remove(0));
		} else
			params.add(null);
		// OPEN_SCOPE
		if (line.get(0) instanceof OpenScope openScope)
			params.add((OpenScope) line.remove(0));
		e.merge(params);
		return e;
	}

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] */
	private static NativeFunction buildNativeFunc() {
		line.remove(0); // FUNC
		NativeFunction e = new NativeFunction(lineID);
		List<Expression> params = new ArrayList<>();
		params.add((Name) line.remove(0));
		line.remove(0); // OpenBrack
		while (line.get(0).is(DATA_TYPE) || line.get(0).is(COMMA)) {
			if (line.remove(0) instanceof ExpectedType t)
				params.add(t);
		}
		line.remove(0); // CloseBrack
		e.merge(params);
		return e;
	}

}
