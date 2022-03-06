package interpreting.modules.disassembler;

import java.util.List;

import interpreting.modules.assembler.Assembler;
import interpreting.modules.parser.Parser.IdxLine;

public abstract class Disassembler {

	public static List<IdxLine> dissassemble(List<IdxLine> lines) {
		System.err.println("Imports are temporarily disabled.");
		return Assembler.assemble(lines);
	}

}
