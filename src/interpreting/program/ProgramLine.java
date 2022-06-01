package interpreting.program;

import static building.types.abstractions.SuperType.START_OF_LINE_TYPE;
import static building.types.specific.KeywordType.ANY;
import static building.types.specific.KeywordType.ELIF;
import static building.types.specific.KeywordType.ELSE;
import static building.types.specific.KeywordType.IF;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import building.expressions.abstractions.MainExpression;
import building.expressions.main.CloseBlock;
import building.expressions.main.functions.Definition;
import building.expressions.main.statements.ConditionalStatement;
import building.expressions.main.statements.ReturnStatement;
import building.expressions.normal.BuilderExpression;
import building.types.abstractions.SpecificType;
import errorhandeling.PseudocodeException;
import importing.filedata.paths.DataPath;
import interpreting.modules.merger.ExpressionMerger;
import launching.Main;

public class ProgramLine {

	final List<BuilderExpression> expressions = new ArrayList<>();

	public final String line;

	public final DataPath dataPath;

	/** The unique lineID that this program generated. */
	public final int lineID;

	private MainExpression main;

	/**
	 * Save a line of code and build its object-expression-representation.
	 *
	 * @param line is the content of this line of code.
	 * @param lineID is the unique identifier.
	 * @param orgLine is the line from the users editor.
	 */
	public ProgramLine(String line, int lineID, DataPath dataPath) {
		this.line = line;
		this.lineID = lineID;
		this.dataPath = dataPath;
	}

	/** Reads the line and constructs an object-expression-notation from the information. */
	void construct() {
		String current = "";
		// Erwartete Ausdr�cke am Zeilenanfang
		SpecificType expectedTypes[] = START_OF_LINE_TYPE.subValues();
		boolean inString = false;
		for (int i = 0; i < line.length(); i++) {
			char c = line.charAt(i);

			if (inString && c == '\\') {
				current += c + "" + line.charAt(i + 1);
				i++;
				continue;
			}
			if (!current.isBlank() && !inString) {
				BuilderExpression be = StringConverter.find(current, c, expectedTypes, this);
				if (be != null) {
					expressions.add(be);
					expectedTypes = be.getExpectedExpressions();
					current = "";
				}
				if (c == ' ')
					continue;
			}
			// Teste nach String- und Chargrenzen
			if (c == '"' || c == '\'')
				inString = !inString;
			current += c;
		}
		assert !inString : "String has to be closed.";
		if (!current.strip().isEmpty()) // Wenn noch ein einzelnes Zeichen am Zeilenende steht.
			expressions.add(StringConverter.create(current.strip(), expectedTypes, this));
		if (expressions.isEmpty())
			throw new AssertionError("Line has to contain atleast one Expression.");
		if (expressions.get(expressions.size() - 1) == null) {
			String exp = expressions.size() >= 2
					? "Expected " + Arrays.toString(expressions.get(expressions.size() - 2).getExpectedExpressions()) + "\nafter \""
							+ expressions.get(expressions.size() - 2) + "\""
					: "";
			throw new PseudocodeException("IllegalCodeFormat",
					"The line couldn't be read completely. The following expressions were recognized: " + expressions + "\n" + exp,
					dataPath);
		}
	}

	/** Returns the last IfStatement or ElifStatement. */
	private ConditionalStatement findLastIf() {
		if (lineID == 0)
			throw new PseudocodeException("InvalidConstruct", "An elif-, any- or else-statement needs a preceding if-statement.", dataPath);
		MainExpression previous = Main.PROGRAM.getLine(lineID - 1).getMainExpression();
		if (previous.is(IF) || previous.is(ELIF) || previous.is(ANY))
			return (ConditionalStatement) previous;
		return Main.PROGRAM.getLine(lineID - 1).findLastIf();
	}

	/** Merges the {@link MainExpression} from the constructed {@link #expressions}. */
	void merge() {
		main = ExpressionMerger.merge(this);
		expressions.clear();
		// Wenn es ein Returnstatement ist, suche die Funktion
		if (main instanceof ReturnStatement ret)
			ret.initFunc(Main.PROGRAM.getLine(lineID - 1).searchForFunc());
		// Connect Conditional blocks.
		else if (main.is(ELIF) || main.is(ANY) || main.is(ELSE)) {
			if (lineID > 0 && !(Main.PROGRAM.getLine(lineID - 1).getMainExpression() instanceof CloseBlock))
				throw new PseudocodeException("MalformedConstruct", //
						main.type + " can only get placed after a closed scope.", //
						dataPath);
			findLastIf().setNextBlock((ConditionalStatement) main);
		}
	}

	/**
	 * Recursivly searches for the next {@link Definition} above this line. Gets used while building the
	 * {@link ReturnStatement}.
	 */
	private Definition searchForFunc() {
		if (main instanceof Definition def)
			return def;
		if (lineID == 0)
			throw new PseudocodeException("InvalidReturn", "Return-Statement has to be declared inside a function.", dataPath);
		return Main.PROGRAM.getLine(lineID - 1).searchForFunc();
	}

	/** Returns the constructed but unmerged {@link #expressions}. */
	public List<BuilderExpression> getExpressions() {
		if (expressions.isEmpty())
			throw new AssertionError("The expressions are either already merged or not even constructed at this point. " + "\nExpressions: "
					+ expressions + "\nLine: " + line + "\nMainExp: " + main);
		return new ArrayList<>(expressions);
	}

	/** Returns the {@link MainExpression} of this line. */
	public MainExpression getMainExpression() {
		if (main != null)
			return main;
		throw new AssertionError("MainExpression in " + dataPath + " is null at this point.");
	}

	@Override
	public String toString() {
		return lineID + "\t" + line;
	}

}
