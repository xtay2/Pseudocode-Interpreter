package interpreting.program;

import static building.types.abstractions.SuperType.*;
import static building.types.specific.KeywordType.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.main.*;
import building.expressions.main.blueprints.*;
import building.expressions.main.functions.*;
import building.expressions.main.statements.*;
import building.expressions.normal.*;
import building.types.abstractions.*;
import errorhandeling.*;
import importing.filedata.paths.*;
import interpreting.modules.merger.*;
import launching.*;

public class ProgramLine {
	
	final List<BuilderExpression> expressions = new ArrayList<>();
	
	public final String line;
	
	/** Always initialised. Can be retrieved with {@link #getDataPath()} */
	private final DataPath dataPath;
	/** Can be retrieved with {@link #getBlueprintPath()} if this line contains no {@link Blueprint} */
	private BlueprintPath blueprintPath;
	
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
					getDataPath());
		}
	}
	
	/** Returns the last IfStatement or ElifStatement. */
	private ConditionalStatement findLastIf() {
		if (lineID == 0)
			throw new PseudocodeException("InvalidConstruct", "An elif-, any- or else-statement needs a preceding if-statement.",
					getDataPath());
		MainExpression previous = Main.PROGRAM.getLine(lineID - 1).getMainExpression();
		if (previous.is(IF) || previous.is(ELIF) || previous.is(ANY))
			return (ConditionalStatement) previous;
		return Main.PROGRAM.getLine(lineID - 1).findLastIf();
	}
	
	/** Merges the {@link MainExpression} from the constructed {@link #expressions}. */
	void merge() {
		main = ExpressionMerger.merge(this);
		expressions.clear();
		if (main instanceof Blueprint bp)
			blueprintPath = bp.getBlueprintPath();
		// Wenn es ein Returnstatement ist, suche die Funktion
		if (main instanceof ReturnStatement ret)
			ret.initFunc(Main.PROGRAM.getLine(lineID - 1).searchForFunc());
		// Connect Conditional blocks.
		else if (main.is(ELIF) || main.is(ANY) || main.is(ELSE)) {
			if (lineID > 0 && !(Main.PROGRAM.getLine(lineID - 1).getMainExpression() instanceof CloseBlock))
				throw new PseudocodeException("MalformedConstruct", //
						main.type + " can only get placed after a closed scope.", //
						getDataPath());
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
			throw new PseudocodeException("InvalidReturn", "Return-Statement has to be declared inside a function.", getDataPath());
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
	
	/**
	 * Returns the most detailed {@link DataPath} available. ({@link #blueprintPath} or
	 * {@link #dataPath} if it doesn't exist.)
	 */
	public DataPath getDataPath() {
		Optional<BlueprintPath> path = getBlueprintPath();
		return path.isPresent() ? path.get() : dataPath;
	}
	
	/**
	 * Returns an {@link Optional} of the {@link BlueprintPath} of this {@link ProgramLine}.
	 *
	 * The {@link Optional} can be {@link Optional#empty()} if the {@link BlueprintPath} of line isn't
	 * inialised. (This should only happen to lines which {@link #main} is an un-build
	 * {@link Blueprint})
	 */
	public Optional<BlueprintPath> getBlueprintPath() {
		if (blueprintPath != null)
			return Optional.of(blueprintPath);
		if (lineID == 0)
			return Optional.empty();
		// Search for Blueprintpath and cache it.
		Optional<BlueprintPath> path = Main.PROGRAM.getLine(lineID - 1).getBlueprintPath();
		if (path.isPresent())
			blueprintPath = path.get();
		return path;
	}
	
	/**
	 * Returns the {@link BlockHolder} that this line lies in.
	 *
	 * <pre>
	 * -It doesn't matter if this {@link #main} is a {@link BlockHolder}, the next outer one gets
	 * returned.
	 * -If this {@link #main} is a {@link Blueprint} (that doesn't lie in a {@link BlockHolder}), null gets returned.
	 * </pre>
	 */
	public BlockHolder getOuterBlock() {
		for (int i = lineID - 1; i >= 0; i--) {
			if (Main.PROGRAM.getLine(i).getMainExpression() instanceof BlockHolder bh)
				return bh;
		}
		return null;
	}
	
	@Override
	public String toString() {
		return lineID + "\t" + line;
	}
	
}
