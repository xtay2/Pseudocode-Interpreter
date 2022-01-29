package parsing.program;

import java.util.ArrayList;
import java.util.Arrays;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.main.CloseScope;
import expressions.main.MainExpression;
import expressions.main.functions.Function;
import expressions.main.statements.ElifConstruct;
import expressions.main.statements.ElifStatement;
import expressions.main.statements.ElseStatement;
import expressions.main.statements.IfStatement;
import expressions.main.statements.ReturnStatement;
import expressions.normal.Expression;
import expressions.normal.Name;
import expressions.normal.brackets.OpenScope;
import expressions.normal.operators.Operator;
import expressions.special.DataType;
import expressions.special.Scope;
import interpreter.Interpreter;
import main.Main;
import parsing.finder.ExpressionFinder;
import parsing.parser.Parser;

public class ProgramLine {

	private final ArrayList<Expression> expressions = new ArrayList<>();
	public final String line;
	public final int lineIdentifier;
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
	 * Connect the Scope of all parameters in this line.
	 *
	 * Find all calls (name, open_bracket, params, close_bracket) and merge them to
	 * one call.
	 */
	private void initScopesAndMain() {
		// Setze die Scopes aller Namen noch vor dem Call-Merge
		Scope scope = searchForScope();
		for (Expression e : expressions)
			if (e instanceof Name)
				((Name) e).initScope(scope);
		// Merge Zeile in eine MainExpression
		main = ValueMerger.buildLine(expressions, lineIdentifier, lineIndex);
		expressions.clear();
	}

	/**
	 * Reads the line and constructs an object-expression-notation from the
	 * information.
	 */
	public void construct() {
		String current = "";
		// Erwartete Ausdrücke am Zeilenanfang
		ExpressionType expectedExpressionTypes[] = { ExpressionType.KEYWORD, ExpressionType.EXPECTED_TYPE, ExpressionType.NAME,
				ExpressionType.CLOSE_SCOPE, ExpressionType.CREMENT };
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
		if(expressions.isEmpty())
			throw new AssertionError("Line has to contain atleast one Expression.");
		initMainExpression();
	}

	/**
	 * Construct and lists an Expression, based on which ExpressionType(s) are
	 * expected.
	 */
	private ExpressionType[] constructExpression(String current, ExpressionType[] expectedExpressionTypes) {
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
	private ElifConstruct findLastIf() {
		if (lineIdentifier == 0)
			throw new IllegalCodeFormatException(lineIndex, "An elif/else Statement needs a predecessing IfStatement.");
		MainExpression previous = Main.PROGRAM.getLine(lineIdentifier - 1).getMainExpression();
		if (previous instanceof IfStatement || previous instanceof ElifStatement)
			return (ElifConstruct) previous;
		return Main.PROGRAM.getLine(lineIdentifier - 1).findLastIf();
	}

	/**
	 * Returns the main-expression of this line.
	 */
	public MainExpression getMainExpression() {
		if (main != null)
			return main;
		throw new AssertionError("MainExpression of line " + lineIndex + " is null at this point.");
	}

	/**
	 * Tells if the current word is a closed expression. The next char is taken to
	 * confirm this choice.
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

	/** Initialises certain types of main expressions, like Scopes. */
	private void initMainExpression() {
		initScopesAndMain();
		// Wenn es ein Returnstatement ist, suche die Funktion
		if (main instanceof ReturnStatement)
			((ReturnStatement) main).setMyFunc(Main.PROGRAM.getLine(lineIdentifier - 1).searchForFunc());
		// Wenn es ein Else-Statement ist, verbinde mit darüberliegendem if.
		else if (main instanceof ElifStatement || main instanceof ElseStatement)
			findLastIf().setNextElse((ElifConstruct) main);
	}

	/**
	 * Recursivly searches for func-declaration. Breaks when encountering the start
	 * of the file.
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
	 * If this line contains a function declaration, the returned scope is the scope
	 * of that function.
	 * 
	 * @see Interpreter#registerFunctions()
	 * @see ProgramLine#initScopesAndMain()
	 */
	public Scope searchForScope() {
		if (main instanceof Function f && f.isNative())
			return Scope.GLOBAL_SCOPE;
		if (main instanceof Scope)
			return (Scope) main;
		if (lineIdentifier == 0)
			return Scope.GLOBAL_SCOPE;
		if (main instanceof CloseScope) {
			ProgramLine match = Main.PROGRAM.getLine(((OpenScope) ((CloseScope) main).getMatch()).lineIdentifier);
			// Da alle Zeilen über dieser bereits ausgewertet wurden, existiert eine
			// MainExpression, die man auswerten kann.
			if (match.getMainExpression() instanceof Function)
				return Scope.GLOBAL_SCOPE;
		}
		return Main.PROGRAM.getLine(lineIdentifier - 1).searchForScope();
	}

	@Override
	public String toString() {
		return lineIdentifier + "\t" + line;
	}
}
