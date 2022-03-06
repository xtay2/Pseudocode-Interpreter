package interpreting.program;

import static building.types.specific.ExpressionType.LITERAL;
import static building.types.specific.ExpressionType.NAME;

import java.util.Arrays;
import java.util.List;

import building.expressions.abstractions.Expression;
import building.expressions.normal.BuilderExpression;
import building.expressions.normal.containers.Name;
import building.types.AbstractType;
import building.types.specific.data.DataType;
import interpreting.exceptions.IllegalCodeFormatException;
import misc.main.Main;

/**
 * 
 * Tells, if a {@link String} can be turned into a {@link BuilderExpression} and returns said
 * {@link Expression}.
 */
public abstract class StringConverter {

	/**
	 * Tells if the current word is a closed expression. The next char is taken to confirm this choice.
	 *
	 * @return {@code true} if current or next is one of ',', '(', ')', ':', '^'
	 */
	public static boolean isNewExpression(String current, char next, AbstractType[] expectedTypes) {
		// String-Literal-Check
		if (ValueBuilder.isString(current))
			return true;
		// ArrayType-Check
		if (DataType.isType(current) && next == '[')
			return false;
		if (isSingleCharOp(next) ^ (current.length() == 1 && isSingleCharOp(current.charAt(0))))
			return matchesExp(current, next, expectedTypes) || Name.isName(current) || ValueBuilder.isLiteral(current);
		return matchesExp(current, next, expectedTypes);
	}

	private static boolean matchesExp(String current, char next, AbstractType[] expectedTypes) {
		try {
			for (AbstractType t : expectedTypes) {
				// Names and Literals get confirmed, when the next char is a singleche-op
				if (t == NAME || t == LITERAL)
					continue;
				if (t.is(current) && !t.is(current + next))
					return true;
			}
		} catch (NullPointerException npe) {
			List<BuilderExpression> line = Main.PROGRAM.stream().filter(e -> !e.expressions.isEmpty()).reduce((fst, snd) -> snd)
					.get().expressions;
			AbstractType trigger = line.get(line.size() - 1).type;
			throw new AssertionError(
					"An expected type was listed as null. This means that it isn't fully initialised in its enum.\nExpected types: "
							+ Arrays.toString(expectedTypes) + "\nInput \"" + current + next + "\"" + "\nTrigger: " + trigger + "-Type"
							+ "\nTrigger expects: " + Arrays.toString(trigger.expected()));
		}
		return false;
	}

	private static boolean isSingleCharOp(char c) {
		for (char o : List.of(' ', '(', ')', '[', ']', '{', '}', '<', '>', '=', ','))
			if (o == c)
				return true;
		return false;
	}

	/** Construct and lists an Expression, based on which ExpressionType(s) are expected. */
	public static AbstractType[] constructExpression(String current, AbstractType[] expectedTypes, ProgramLine line) {
		final int lineID = line.lineID;
		final int orgLine = line.orgLine;
		final List<BuilderExpression> expressions = line.expressions;
		BuilderExpression exp = find(current, lineID, expectedTypes);
		if (exp == null)
			throw new IllegalCodeFormatException(orgLine, "No matching Expression was found for: " //
					+ current + "\n" //
					+ "Expected " + (expectedTypes.length == 0 ? "a linebreak" : Arrays.toString(expectedTypes))
					+ (expressions.isEmpty() ? "" : " after " + expressions.get(expressions.size() - 1)) + ".\n" //
					+ "Current state of line: \n" + line + "\n" + expressions);
		expressions.add(exp);
		return exp.getExpectedExpressions();
	}

	/**
	 * Find the matching Expression out of the input and what is expected.
	 *
	 * @param current  is the currently read string.
	 * @param expected is an array of expected expressions.
	 * @return the matching Expression
	 * @throws IllegalArgumentException if no matching expression was found.
	 */
	public static BuilderExpression find(String current, int lineID, AbstractType... expected) {
		for (AbstractType expT : expected) {
			BuilderExpression exp = expT.create(current, lineID);
			if (exp != null)
				return exp;
		}
		return null;
	}
}
