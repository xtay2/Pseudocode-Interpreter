package interpreting.program;

import static building.types.specific.DynamicType.*;
import static misc.util.Regex.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.normal.*;
import building.expressions.normal.containers.name.*;
import building.types.abstractions.*;
import errorhandeling.*;
import misc.helper.*;
import runtime.datatypes.*;

/**
 * Tells, if a {@link String} can be turned into a {@link BuilderExpression} and returns said
 * {@link Expression}.
 */
public abstract class StringConverter {
	
	/**
	 * Creates a {@link BuilderExpression} from a {@link String}, when only the expected types are
	 * known.
	 *
	 * @param current is the {@link String}.
	 * @param expectedTypes are the types that the {@link BuilderExpression} can have.
	 * @param line is the {@link ProgramLine} that calls this method.
	 * @return a {@link BuilderExpression} or null, if the {@link String} doesn't match any type.
	 */
	public static BuilderExpression create(String arg, SpecificType[] expectedTypes, ProgramLine line) {
		return find(arg, ' ', expectedTypes, line);
	}
	
	/**
	 * Creates a {@link BuilderExpression} from a {@link String}, when only the expected types are
	 * known.
	 *
	 * @param current is the {@link String}.
	 * @param next is the follow-up-character after the current {@link String}.
	 * @param expectedTypes are the types that the {@link BuilderExpression} can have.
	 * @param line is the {@link ProgramLine} that calls this method.
	 * @return a {@link BuilderExpression} or null, if the {@link String} doesn't match any type.
	 */
	public static BuilderExpression find(String current, char next, SpecificType[] expectedTypes, ProgramLine line) {
		SpecificType st = isStructurePart(current, next, expectedTypes, line);
		if (st != null)
			return st.create(current, line.lineID);
		for (SpecificType t : expectedTypes) {
			if (LITERAL.is(t) && isValuePart(current, next))
				return LITERAL.create(current, line.lineID);
			else if (NAME.is(t) && isNamePart(current, next))
				return NAME.create(current, line.lineID);
		}
		if (next == ' ') {
			throw new PseudocodeException("IllegalCodeFormat", //
					"\"" + current + "\" didn't match any expected pattern. ", //
					ProgramHelper.underlineFirstRunnable(line.line, current + "(?=\\" + next + ")"), //
					line.getDataPath());
		}
		return null;
	}
	
	/** Returns true if just the current part is a {@link Value} and next is a valid delimiter. */
	private static boolean isValuePart(String current, char next) {
		if (next == '.') // Special case for decimal numbers
			return false;
		return ValueBuilder.isLiteral(current) && !ValueBuilder.isLiteral(current + next) && !WR.matches(current + next);
	}
	
	/** Returns true if just the current part is a {@link Name} and next is a valid delimiter. */
	private static boolean isNamePart(String current, char next) {
		return WR.matches(current) && !WR.matches(current + next) && !Name.isAlphaNumKeyword(current + next);
	}
	
	/**
	 * Filters through the expected types to find the matching {@link SpecificType} for the
	 * {@link String}.
	 *
	 * @param current is the passed {@link String}.
	 * @param next is the next {@link Character}.
	 * @param expectedTypes is the array of possible {@link SpecificType}s.
	 * @param line is the {@link ProgramLine} from which this method is called.
	 * @return a {@link SpecificType} or null if nothing was found.
	 */
	private static SpecificType isStructurePart(String current, char next, SpecificType[] expectedTypes, ProgramLine line) {
		List<SpecificType> potMatches = new ArrayList<>();
		for (SpecificType t : expectedTypes) {
			if (t.toString().startsWith(current) || t.is(NAME))
				potMatches.add(t);
		}
		// Check if keyword is just part of a name
		if (potMatches.contains(NAME)) {
			if (WR.matches(current + next))
				return null;
			else
				potMatches.remove(NAME);
		}
		// If theres a potential match that starts with (current + next) or is equal to
		// current
		potMatches = potMatches.stream().filter(e -> e.toString().startsWith(current + next) || e.toString().equals(current)).toList();
		if (potMatches.size() == 1 && potMatches.get(0).toString().equals(current))
			return potMatches.get(0);
		return null;
	}
}
