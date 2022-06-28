package runtime.natives;

import static misc.constants.TypeConstants.*;

import java.io.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import runtime.datatypes.*;
import runtime.datatypes.numerical.*;
import runtime.datatypes.textual.*;

/**
 * The definitions of all native functions.
 *
 * @deprecated This class should get heavily reworked, so that the native functions are in a
 * seperate tree.
 */
@Deprecated
public final class SystemFunctions {
	
	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {
		
		/** Terminates the program. */
		EXIT("exit", TEXT),
		
		/** Prints something in the console. */
		PRINT("print", TEXT),
		
		/** Reads something from the console. */
		READ("read"),
		
		/** Returns a fractional representation of a number. */
		AS_RATIONAL("asRational", NR),
		
		/** Returns a random {@link NumberValue} in a specified range. */
		RAND_NR("randNr", NR, NR),
		
		/** Returns a timestamp of the current system clock in nanoseconds. */
		TIMESTAMP("timestamp");
		
		public final String name;
		public final DataType[] argTypes;
		
		private SYSTEM_FUNCTION(String name, DataType... argTypes) {
			this.name = name;
			this.argTypes = argTypes;
		}
	}
	
	/**
	 * Call a specific native func.
	 *
	 * @param func is the {@link SYSTEM_FUNCTION} that describes this native func.
	 * @param params are optional passed pararameters.
	 * @return is an optional return value. May be null.
	 * @throws NonExpressionException for Casting or the wrong amount of params.
	 */
	public static Value callSystemFunc(SYSTEM_FUNCTION func, ValueHolder... params) throws NonExpressionException {
		if (params.length != func.argTypes.length) {
			throw new NonExpressionException("NativeCall", "Called the native definition " + func.name + " with " + params.length
					+ " although " + func.argTypes.length + " were expected.");
		}
		return switch (func) {
			case PRINT -> print(params[0].asText());
			case READ -> read();
			case EXIT -> exit(params[0].asText());
			case AS_RATIONAL -> asRational(params[0].asNr());
			case RAND_NR -> randNr(params[0].asNr(), params[1].asNr());
			case TIMESTAMP -> timestamp();
		};
	}
	
	/** Returns the matching system-function for this name. */
	public static SYSTEM_FUNCTION getSystemFunction(Name name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name.getNameString()))
				return f;
		throw new PseudocodeException("UnknownNativeFunc", "There is no native function called " + name + ".", name.getBlueprintPath());
	}
	
	/** native func exit(text) */
	private static Value exit(TextValue exitMsg) {
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}
	
	/** native func read(text) */
	private static Value print(TextValue msg) {
		System.out.println(msg.raw());
		return null;
	}
	
	/** native func read() -> text */
	private static TextValue read() {
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		try {
			return new TextValue(reader.readLine());
		} catch (IOException e) {
			return null;
		}
	}
	
	/** Implementation: native func asRational(nr) -> text */
	private static TextValue asRational(NumberValue nr) {
		if (nr instanceof DecimalValue d)
			return new TextValue(d.asRational());
		return nr instanceof IntValue i ? new TextValue(i.value.toString() + "/1") : nr.asText();
	}
	
	/** Implementation: native func randNr(nr, nr) -> nr */
	private static NumberValue randNr(NumberValue a, NumberValue b) {
		throw new AssertionError("This feature isn't implemented yet.");
	}
	
	/** Implementation: native func timestamp() -> int */
	private static IntValue timestamp() {
		return new IntValue(System.nanoTime());
	}
}
