package modules.parser.program;

import static types.SuperType.DATA_TYPE;
import static types.SuperType.FLAG_TYPE;
import static types.SuperType.KEYWORD_TYPE;
import static types.specific.ExpressionType.CLOSE_SCOPE;
import static types.specific.ExpressionType.DECREMENT;
import static types.specific.ExpressionType.INCREMENT;
import static types.specific.ExpressionType.NAME;
import static types.specific.KeywordType.ELIF;
import static types.specific.KeywordType.ELSE;
import static types.specific.KeywordType.IF;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.GlobalScope;
import expressions.abstractions.MainExpression;
import expressions.abstractions.Scope;
import expressions.abstractions.ScopeHolder;
import expressions.main.CloseScope;
import expressions.main.functions.Function;
import expressions.main.functions.NativeFunction;
import expressions.main.functions.Returnable;
import expressions.main.statements.ConditionalStatement;
import expressions.main.statements.ReturnStatement;
import expressions.normal.brackets.OpenScope;
import expressions.normal.operators.Operator;
import main.Main;
import modules.finder.ExpressionFinder;
import modules.interpreter.Interpreter;
import modules.parser.Parser;
import types.AbstractType;
import types.specific.DataType;

public class ProgramLine {

	private final ArrayList<Expression> expressions = new ArrayList<>();
	final String line;
	private final int lineIdentifier;
	public final int lineIndex;
	private MainExpression main;

	/**
	 * Save a line of code and build its object-expression-representation.
	 *
	 * @param line is the content of this line of code.
	 */
	public ProgramLine(String line, int lineIdentifier, int lineIndex) {
		this.line = line;
		this.lineIdentifier = lineIdentifier;
		this.lineIndex = lineIndex;
	}

	/**
	 * Reads the line and constructs an object-expression-notation from the information.
	 */
	void construct() {
		String current = "";
		// Erwartete Ausdrücke am Zeilenanfang
		AbstractType expectedExpressionTypes[] = { KEYWORD_TYPE, DATA_TYPE, FLAG_TYPE, NAME, CLOSE_SCOPE, INCREMENT, DECREMENT };
		boolean inString = false;
		for (int i = 0; i < line.length(); i++) {
			char c = line.charAt(i);

			if (inString && c == '\\') {
				current += c + "" + line.charAt(i + 1);
				i++;
				continue;
			}

			// Checke nach single line comments
			if (!inString && c == Parser.SINGLE_LINE_COMMENT)
				break;

			if (!inString)
				current = current.strip();

			// Neue Expression wenn c ' ', ',' oder '(' ist.
			if (!current.isBlank() && !inString && isNewExpression(current, c)) {
				expectedExpressionTypes = constructExpression(current, expectedExpressionTypes);
				current = "";
			}
			// Teste nach Stringgrenzen
			if (c == '"')
				inString = !inString;
			current += c;
		}
		if (inString)
			throw new IllegalCodeFormatException(lineIndex, "String has to be closed.");
		if (!current.strip().isEmpty()) // Wenn noch ein einzelnes Zeichen am Zeilenende steht.
			constructExpression(current.strip(), expectedExpressionTypes);
		if (expressions.isEmpty())
			throw new AssertionError("Line has to contain atleast one Expression.");
	}

	/**
	 * Tells if the current word is a closed expression. The next char is taken to confirm this choice.
	 *
	 * @return {@code true} if current or next is one of ',', '(', ')', ':', '^'
	 */
	private boolean isNewExpression(String current, char next) {
		if (Operator.isOperator(String.valueOf(next)))
			return !Operator.isOperator(current);
		if (current.equals("++") || current.equals("--"))
			return true;
		if ((DataType.isType(current) && next == '[') || (DataType.isType(current.replace("[", "")) && next == ']'))
			return false;
		char oneCharExpressions[] = { ',', '(', ')', ':', '[', ']', ';' };
		for (char c : oneCharExpressions)
			if (current.charAt(0) == c || next == c)
				return true;
		return next == ' ';
	}

	/**
	 * Construct and lists an Expression, based on which ExpressionType(s) are expected.
	 */
	private AbstractType[] constructExpression(String current, AbstractType[] expectedExpressionTypes) {
		Expression exp = ExpressionFinder.find(current, expectedExpressionTypes, lineIdentifier);
		if (exp == null)
			throw new IllegalCodeFormatException(lineIndex, "No matching Expression was found for: " //
					+ current + "\n" //
					+ "Expected " + (expectedExpressionTypes.length == 0 ? "a linebreak" : Arrays.toString(expectedExpressionTypes))
					+ (expressions.isEmpty() ? "." : " after " + expressions.get(expressions.size() - 1)) + ".\n" //
					+ "Current state of line: \n" + line + "\n" + expressions);
		expressions.add(exp);
		return exp.getExpectedExpressions();
	}

	/** Returns the last IfStatement or ElifStatement. */
	private ConditionalStatement findLastIf() {
		if (lineIdentifier == 0)
			throw new IllegalCodeFormatException(lineIndex, "An elif/else Statement needs a predecessing IfStatement.");
		MainExpression previous = Main.PROGRAM.getLine(lineIdentifier - 1).getMainExpression();
		if (previous.is(IF) || previous.is(ELIF))
			return (ConditionalStatement) previous;
		return Main.PROGRAM.getLine(lineIdentifier - 1).findLastIf();
	}

	/** Merges the {@link MainExpression} from the constructed {@link #expressions}. */
	void merge() {
		main = ValueMerger.buildLine(expressions, lineIdentifier, lineIndex);
		expressions.clear();
		// Wenn es ein Returnstatement ist, suche die Funktion
		if (main instanceof ReturnStatement)
			((ReturnStatement) main).initFunc(Main.PROGRAM.getLine(lineIdentifier - 1).searchForFunc());
		// Wenn es ein Else-Statement ist, verbinde mit darüberliegendem if.
		else if (main.is(ELIF) || main.is(ELSE))
			findLastIf().setNextElse((ConditionalStatement) main);
	}

	/**
	 * Recursivly searches for func-declaration. Breaks when encountering the start of the file.
	 */
	private Function searchForFunc() {
		if (main instanceof Function)
			return (Function) main;
		if (lineIdentifier == 0)
			throw new IllegalCodeFormatException(lineIndex, "Return-Statement has to be declared inside a function.");
		return Main.PROGRAM.getLine(lineIdentifier - 1).searchForFunc();
	}

	/**
	 * Recursivly searches for the scope of this line.
	 * 
	 * If this line contains a function declaration, the returned scope is the scope of that function.
	 * 
	 * @see Interpreter#registerFunctions()
	 * @see ProgramLine#initScopesAndMain()
	 */
	public Scope searchForScope() {
		if (main instanceof Returnable)
			return GlobalScope.GLOBAL;
		if (main instanceof ScopeHolder s)
			return s.getScope();
		if (lineIdentifier == 0)
			return GlobalScope.GLOBAL;
		if (main instanceof CloseScope) {
			ProgramLine match = Main.PROGRAM.getLine(((OpenScope) ((CloseScope) main).getMatch()).lineIdentifier);
			// Da alle Zeilen über dieser bereits ausgewertet wurden, existiert eine
			// MainExpression, die man auswerten kann.
			if (match.getMainExpression() instanceof Function)
				return GlobalScope.GLOBAL;
		}
		return Main.PROGRAM.getLine(lineIdentifier - 1).searchForScope();
	}

	/** Returns the constructed but unmerged {@link #expressions}. */
	public List<Expression> getExpressions() {
		if (expressions.isEmpty())
			throw new AssertionError("The expressions are either already merged or not even constructed at this point. Line: " + line);
		return new ArrayList<>(expressions);
	}

	/** Returns the {@link MainExpression} of this line. */
	public MainExpression getMainExpression() {
		if (main != null)
			return main;
		throw new AssertionError("MainExpression of line " + lineIndex + " is null at this point.");
	}

	@Override
	public String toString() {
		return lineIdentifier + "\t" + line;
	}

}
