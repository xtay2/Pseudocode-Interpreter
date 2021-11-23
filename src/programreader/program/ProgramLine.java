package programreader.program;

import java.util.ArrayList;
import java.util.Arrays;

import exceptions.DeclarationException;
import programreader.expressions.main.CloseBlock;
import programreader.expressions.main.functions.Function;
import programreader.expressions.main.statements.ReturnStatement;
import programreader.expressions.normal.Name;
import programreader.expressions.normal.OpenBlock;
import programreader.expressions.special.Expression;
import programreader.expressions.special.MainExpression;
import programreader.expressions.special.Scope;
import programreader.finder.ExpressionFinder;
import programreader.parser.Parser;

import static helper.Output.*;

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
		ExpressionType expectedExpressionTypes[] = { ExpressionType.KEYWORD, ExpressionType.TYPED_VAR, ExpressionType.NAME,
				ExpressionType.CLOSE_BLOCK };
		boolean inString = false;
		for (int i = 0; i < line.length(); i++) {
			char c = line.charAt(i);
			// Checke nach single line comments
			if (c == Parser.SINGLE_LINE_COMMENT)
				break;

			// Überspringe escaped symbols
			if (inString && c == '\\') {
				i++;
				continue;
			}
			// Neue Expression wenn c ' ', ',' oder '(' ist.
			if (current.length() > 0 && !inString && isNewExpression(current.charAt(0), c)) {
				expectedExpressionTypes = constructExpression(current.strip(), expectedExpressionTypes);
				current = "";
			}
			// Teste nach Stringgrenzen
			if (c == '"')
				inString = !inString;
			current += c;
		}
		if (!"".equals(current.strip())) // Wenn noch ein einzelnes Zeichen am Zeilenende steht.
			constructExpression(current.strip(), expectedExpressionTypes);
		mergeLine();
	}

	/** Merge the important information into the Main Expression. */
	private void mergeLine() {
		print(expressions.toString());
		connectExpressions();
		main = findMainExpression();
		// Wenn es eine schließende Klammer suche ihren Partner.
		if (main instanceof CloseBlock)
			program.getLine(lineIndex - 1).connectBlock((CloseBlock) main, -1);
		// Wenn es ein Returnstatement ist, suche die Funktion
		else if (main instanceof ReturnStatement)
			((ReturnStatement) main).setMyFunc(program.getLine(lineIndex - 1).searchForFunc());
		main.build(expressions.toArray(new Expression[expressions.size()]));
	}

	/**
	 * Connect the Scopes of this line to all names.
	 * 
	 * Find all calls (name, open_bracket, params, close_bracket) and merge them to
	 * one call.
	 */
	private void connectExpressions() {
		// Setze die Scopes aller Namen noch vor dem Call-Merge
		Scope scope = searchForScope();
		for (Expression e : expressions) {
			if (e instanceof Name)
				((Name) e).initScope(scope);
		}
		expressions = ValueBuilder.buildLine(expressions, lineIndex);
		// Funktionen dürfen nicht in anderen Funktionen definiert werden.
		if (scope != Scope.GLOBAL_SCOPE && expressions.get(0) instanceof Function && !scope.isOneLineStatement())
			throw new DeclarationException("A function (" + scope.getScopeName() + ") cannot be declared inside another function.");
	}

	/** Finds and returns the MainExpression of this line. */
	private MainExpression findMainExpression() {
		for (Expression e : expressions) {
			if (e.isMainExpression())
				return (MainExpression) e;
		}
		throw new IllegalStateException("Line doesn't contain a main-expression: " + line + "\n" + expressions);
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
		if (lineIndex > 0)
			program.getLine(lineIndex - 1).connectBlock(close, brackets);
		else
			throw new IllegalStateException("Matching Bracket wasn't found.");
	}

	/**
	 * Recursivly searches for the scope.
	 */
	private Scope searchForScope() {
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
			throw new IllegalStateException("Return-Statement has to be declared inside a function.");
		return program.getLine(lineIndex - 1).searchForFunc();
	}

	/**
	 * Tells if the current word is an closed expression. The next char is taken to
	 * confirm this choice.
	 * 
	 * @return {@code true} if current or next is one of [',', '(', ')', ':']
	 */
	private boolean isNewExpression(char current, char next) {
		char oneCharExpressions[] = { ',', '(', ')', ':' };
		for (char c : oneCharExpressions) {
			if (current == c || next == c)
				return true;
		}
		return next == ' ';
	}

	/**
	 * Construct and list an Expression, based on which ExpressionType(s) are
	 * expected.
	 */
	private ExpressionType[] constructExpression(String current, ExpressionType[] expectedExpressionTypes) {
		Expression exp = ExpressionFinder.find(current, expectedExpressionTypes, lineIndex);
		if (exp == null)
			throw new IllegalArgumentException(
					"No matching Expression was found for: " + current + "\n" + "Expected " + Arrays.toString(expectedExpressionTypes)
							+ (expressions.isEmpty() ? "." : " after " + expressions.get(expressions.size() - 1)) + "\n"
							+ "Current state of line: " + expressions);
		expressions.add(exp);
		return exp.getExpectedExpressions();
	}

	/**
	 * Returns the main-expression of this line.
	 */
	public MainExpression getExpression() {
		return main;
	}

	@Override
	public String toString() {
		return lineIndex + "\t" + line;
	}
}
