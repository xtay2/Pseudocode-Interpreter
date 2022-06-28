package building.expressions.normal.containers.name;

import static building.types.specific.DynamicType.*;
import static misc.util.Regex.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.types.abstractions.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import building.types.specific.operators.*;
import errorhandeling.*;
import launching.*;

/**
 * Every piece of text that isn't predefined by the Interpreter via Keywords, Operators, etc...
 * (Wrapper-{@link Expression} for {@link String}s).
 */
public sealed abstract class Name extends Expression implements ValueChanger permits ClassName, ConstName, VarName {
	
	private final String name;
	
	/** Creates a {@link Name} from a {@link String}. */
	Name(int lineID, String name) {
		super(lineID, NAME);
		assert name != null : "Name cannot be null.";
		this.name = name;
		if (isAlphaNumKeyword(name) && !name.equals(KeywordType.MAIN.toString())) {
			throw new PseudocodeException("InvalidName", //
					"A name cannot match a keyword. Was: " + name, //
					getBlueprintPath() //
			);
		}
	}
	
	/**
	 * This method constructs a Name.
	 *
	 * @param arg is the {@link String} of this {@link Name}.
	 * @return a newely constructed {@link Name}.
	 */
	public static Name generateName(int lineID, String arg) {
		if (WR_LC.matches(arg))
			return new VarName(lineID, arg);
		if (BP.matches(arg))
			return new ClassName(lineID, arg);
		if (WR_UC.matches(arg))
			return new ConstName(lineID, arg);
		throw new PseudocodeException("NameMatch", //
				"\"" + arg + "\" doesn't match the any name-patterns. Should be an alphanumeric string.", //
				Main.PROGRAM.getLine(lineID).getDataPath());
	}
	
	/** Returns true, if the passed string matches any alphanumerical keyword. */
	public static boolean isAlphaNumKeyword(String arg) {
		//@formatter:off
		return SpecificType.equalsString(arg, KeywordType.class)
			|| SpecificType.equalsString(arg, SingleType.class)
			|| SpecificType.equalsString(arg, FlagType.class)
			|| SpecificType.equalsString(arg, InfixOpType.class);
		//@formatter:on
	}
	
	/**
	 * Returns the String-Representation of this {@link Name}.
	 *
	 * This is the base for all instances of {@link NameHolder#getNameString()}.
	 */
	@Override
	public final String getNameString() { return name; }
	
	@Override
	public final Name getName() { return this; }
	
	@Override
	public boolean equals(Object obj) {
		return obj instanceof Name n ? name.equals(n.name) : false;
	}
	
	@Override
	public String toString() {
		return getNameString();
	}
}
