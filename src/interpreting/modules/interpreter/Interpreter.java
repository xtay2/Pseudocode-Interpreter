package interpreting.modules.interpreter;

import static misc.supporting.Output.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.main.functions.*;
import building.expressions.main.statements.*;
import building.expressions.normal.containers.*;
import building.expressions.possible.allocating.*;
import building.types.specific.*;
import importing.filedata.paths.*;
import interpreting.program.*;
import launching.*;
import runtime.defmanager.*;

public final class Interpreter {
	
	/**
	 * Registeres every {@link Variable} and {@link Definition} and starts the interpreting-process by
	 * calling the {@link MainFunction}.
	 *
	 * @param program is the program that gets interpreted.
	 *
	 * @see Main#main
	 */
	public static void interpret() {
		// INIT
		registerGlobalVars();
		// RUNTIME
		print("\nStarting Program: " + UNDERLINE);
		// Call Main
		DefManager.get(KeywordType.MAIN.toString(), 0, Main.PROGRAM.stream() //
				.filter(l -> l.getMainExpression() instanceof MainFunction) //
				.findFirst().get().getBlueprintPath().get().blueprint).call();
	}
	
	/**
	 * Executes a MainExpression.
	 *
	 * @param i the line of the MainExpression.
	 * @param params are the passed parameters
	 *
	 * @return false if this function shouldn't call any other functions afterwards.
	 * {@link ReturnStatement#execute}
	 */
	public static boolean execute(int i) {
		MainExpression m = Main.PROGRAM.getLine(i).getMainExpression();
		print(m.getBlueprintPath() + ": " + m.toString());
		if (m instanceof ScopeHolder) {
			ScopeManager.STACK.allocate();
			boolean callNext = m.execute();
			ScopeManager.STACK.free();
			return callNext;
		}
		return m.execute();
	}
	
	/**
	 * Registers and initialises all static Variables in the global scope.
	 *
	 * (Outside of functions).
	 *
	 * @see ProgramLine
	 * @see Declaration
	 */
	private static void registerGlobalVars() {
		for (ProgramLine l : Main.PROGRAM) {
			Optional<BlueprintPath> bpp = l.getBlueprintPath();
			if (bpp.isEmpty())
				continue;
			if (l.getMainExpression() instanceof Declaration d && l.getOuterBlock().equals(bpp.get())) {
				print("Registering global Var " + d.getNameString() + " in line: " + d.getBlueprintPath());
				d.getValue();
			}
		}
	}
}
