package misc.constants;

import building.expressions.abstractions.*;
import building.types.specific.datatypes.*;

/**
 * A collection of constants, used for basic casting.
 */
public abstract class TypeConstants {
	
	private TypeConstants() {
		// Dead Constructor
	}
	
	// Nullable Types @formatter:off
	public static final DataType VAR  = new DataType(SingleType.VAR,  true);
	public static final DataType TEXT = new DataType(SingleType.TEXT, true);
	public static final DataType CHAR = new DataType(SingleType.CHAR, true);
	public static final DataType NR   = new DataType(SingleType.NR,   true);
	public static final DataType INT  = new DataType(SingleType.INT,  true);
	public static final DataType BOOL = new DataType(SingleType.BOOL, true);

	// Nullable-Arrays of unbound length
	public static final DataType VAR_ARR  = new DataType(SingleType.VAR,  true, Range.UNBOUNDED);
	public static final DataType TEXT_ARR = new DataType(SingleType.TEXT, true, Range.UNBOUNDED);
	public static final DataType CHAR_ARR = new DataType(SingleType.CHAR, true, Range.UNBOUNDED);
	public static final DataType NR_ARR   = new DataType(SingleType.NR,   true, Range.UNBOUNDED);
	public static final DataType INT_ARR  = new DataType(SingleType.INT,  true, Range.UNBOUNDED);
	public static final DataType BOOL_ARR = new DataType(SingleType.BOOL, true, Range.UNBOUNDED);
	//@formatter:on
}
