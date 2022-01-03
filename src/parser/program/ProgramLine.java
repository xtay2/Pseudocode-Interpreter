package parser.program;

import static helper.Output.print;

import java.util.ArrayList;
import java.util.Arrays;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.main.CloseBlock;
import expressions.main.functions.Function;
import expressions.main.statements.ElifConstruct;
import expressions.main.statements.ElifStatement;
import expressions.main.statements.ElseStatement;
import expressions.main.statements.IfStatement;
import expressions.main.statements.ReturnStatement;
import expressions.normal.Name;
import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.Type;
import interpreter.Interpreter;
import parser.Parser;
import parser.finder.ExpressionFinder;

public class ProgramLine {

	public final String line;

	public final Program program;
	public final int lineIndex;
	private ArrayList<Expression> expressions = new ArrayList<>();
	private MainExpression main;

	/**
	 * Save a line of code and build its object-expression-representation.
	 *
	 * @param line is the content of this line of code.
	 */
	public ProgramLine(String line, int lineIndex, Program program) {
		this.program = program;
		this.line = line;
		this.lineIndex = lineIndex;
		construct();
	}

	/**
	 * Reads the line and constructs an object-expression-notation from the
	 * information.
	 */
	private void construct() {
		String current = "";
		// Erwartete Ausdrücke am Zeilenanfang
		ExpressionType expectedExpressionTypes[] = { ExpressionType.KEYWORD, ExpressionType.VAR_TYPE, ExpressionType.NAME,
				ExpressionType.CLOSE_BLOCK };
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
			throw new IllegalCodeFormatException("String has to be closed.");
		if (!"".equals(current.strip())) { // Wenn noch ein einzelnes Zeichen am Zeilenende steht.
			constructExpression(current.strip(), expectedExpressionTypes);
		}
		mergeLine();
	}

	/**
	 * Tells if the current word is a closed expression. The next char is taken to
	 * confirm this choice.
	 *
	 * @return {@code true} if current or next is one of ',', '(', ')', ':', '^'
	 */
	private boolean isNewExpression(String current, char next) {
		if ((Type.isType(current) && next == '[') || (Type.isType(current.replace("[", "")) && next == ']'))
			return false;
		char oneCharExpressions[] = { ',', '(', ')', ':', '[', ']', '^', ';' };
		for (char c : oneCharExpressions)
			if (current.charAt(0) == c || next == c)
				return true;
		return next == ' ';
	}

	/** Merge the important information into the Main Expression. */
	private void mergeLine() {
		connectExpressions();
		main = findMainExpression();
		// Wenn es eine schließende Klammer suche ihren Partner.
		if (main instanceof CloseBlock)
			program.getLine(lineIndex - 1).connectBlock((CloseBlock) main, -1);
		// Wenn es ein Returnstatement ist, suche die Funktion
		else if (main instanceof ReturnStatement)
			((ReturnStatement) main).setMyFunc(program.getLine(lineIndex - 1).searchForFunc());
		// Wenn es ein Else-Statement ist, verbinde mit darüberliegendem if.
		else if (main instanceof ElifStatement || main instanceof ElseStatement)
			findLastIf().setNextElse((ElifConstruct) main);
		main.build(expressions.toArray(new Expression[expressions.size()]));
		print(expressions.toString());
	}

	/** Returns the last IfStatement or ElifStatement. */
	private ElifConstruct findLastIf() {
		if (lineIndex == 0)
			throw new IllegalCodeFormatException("An elif/else Statement needs a predecessing IfStatement.");
		MainExpression previous = program.getLine(lineIndex - 1).getMainExpression();
		if (previous instanceof IfStatement || previous instanceof ElifStatement)
			return (ElifConstruct) previous;
		return program.getLine(lineIndex - 1).findLastIf();
	}

	/** Finds and returns the MainExpression of this line. */
	private MainExpression findMainExpression() {
		for (Expression e : expressions)
			if (e.isMainExpression())
				return (MainExpression) e;
		throw new IllegalCodeFormatException("Line doesn't contain a main-expression: " + line + "\n" + expressions);
	}

	/**
	 * Connect the Scope of all parameters in this line.
	 *
	 * Find all calls (name, open_bracket, params, close_bracket) and merge them to
	 * one call.
	 */
	private void connectExpressions() {
		// Setze die Scopes aller Namen noch vor dem Call-Merge
		Scope scope = searchForScope();
		for (Expression e : expressions)
			if (e instanceof Name)
				((Name) e).initScope(scope);
		// Merge Zeile in eine function/declaration/call...
		expressions = ValueBuilder.buildLine(expressions, lineIndex);
	}

	/**
	 * Recursivly connects Block-Brackets.
	 *
	 * @param close    is the closing bracket.
	 * @param brackets is the count of brackets inbetween.
	 */
	private void connectBlock(CloseBlock close, int brackets) {
		Expression last = expressions.get(expressions.size() - 1);
		if (last instanceof CloseBlock)
			brackets--;
		else if (last instanceof OpenBlock) {
			brackets++;
			if (brackets == 0) {
				((OpenBlock) last).setMyMatch(close);
				close.setMyMatch((OpenBlock) last);
				return;
			}
		}
		if (lineIndex <= 0)
			throw new IllegalCodeFormatException("Matching Bracket wasn't found.");
		program.getLine(lineIndex - 1).connectBlock(close, brackets);
	}

	/**
	 * Recursivly searches for the scope of this line.
	 * 
	 * If this line contains a function declaration, the returned scope is the scope
	 * of that function.
	 * 
	 * @see Interpreter#registerFunctions()
	 * @see ProgramLine#connectExpressions()
	 */
	public Scope searchForScope() {
		if (expressions.get(0) instanceof Scope)
			return (Scope) expressions.get(0);
		if (main instanceof Scope)
			return (Scope) main;
		if (lineIndex == 0)
			return Scope.GLOBAL_SCOPE;
		if (main instanceof CloseBlock) {
			String line = program.readLine(((OpenBlock) ((CloseBlock) main).getMatch()).line);
			if (line.startsWith(KeywordType.FUNC.keyword) || line.startsWith(KeywordType.MAIN.keyword))
				return Scope.GLOBAL_SCOPE;
		}
		return program.getLine(lineIndex - 1).searchForScope();
	}

	/**
	 * Recursivly searches for func-declaration. Breaks when encountering the start
	 * of the file.
	 */
	private Function searchForFunc() {
		if (main instanceof Function)
			return (Function) main;
		if (lineIndex == 0)
			throw new IllegalCodeFormatException("Return-Statement has to be declared inside a function.");
		return program.getLine(lineIndex - 1).searchForFunc();
	}

	/**
	 * Construct and lists an Expression, based on which ExpressionType(s) are
	 * expected.
	 */
	private ExpressionType[] constructExpression(String current, ExpressionType[] expectedExpressionTypes) {
		Expression exp = ExpressionFinder.find(current, expectedExpressionTypes, lineIndex);
		if (exp == null)
			throw new IllegalCodeFormatException("No matching Expression was found for: " //
					+ current + "\n" //
					+ "Expected " + (expectedExpressionTypes.length == 0 ? "a linebreak" : Arrays.toString(expectedExpressionTypes))
					+ (expressions.isEmpty() ? "." : " after " + expressions.get(expressions.size() - 1)) + ".\n" //
					+ "Current state of line: " + expressions);
		expressions.add(exp);
		return exp.getExpectedExpressions();
	}

	/**
	 * Returns the main-expression of this line.
	 */
	public MainExpression getMainExpression() {
		return main;
	}

	@Override
	public String toString() {
		return lineIndex + "\t" + line;
	}
}
