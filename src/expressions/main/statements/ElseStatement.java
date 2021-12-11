package expressions.main.statements;

import static helper.Output.print;

import expressions.main.CloseBlock;
import expressions.normal.OpenBlock;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;
import parser.program.ExpressionType;

public class ElseStatement extends MainExpression implements ElifConstruct {

	private OpenBlock block = null;

	public ElseStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.ONE_LINE_STATEMENT, ExpressionType.OPEN_BLOCK);
	}

	@Override
	public void build(Expression... args) {
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing Else-Statement.");
		if (!doExecuteNext)
			throw new IllegalStateException("An else-statement has to be able to call the next line.");
		VarManager.registerScope(this);
		if (!Interpreter.execute(line + 1, !isOneLineStatement())) {
			VarManager.deleteScope(this);
			return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
		}
		VarManager.deleteScope(this);
		return true; //Lasse das if-Statement die nächste Zeile ausführen.
	}

	@Override
	public int getStart() {
		return line;
	}

	@Override
	public int getEnd() {
		return isOneLineStatement() || block.getMatch() == null ? line + 2 : ((CloseBlock) block.getMatch()).line + 1;
	}

	@Override
	public String getScopeName() {
		return "else" + getStart() + "-" + getEnd();
	}

	@Override
	public boolean isOneLineStatement() {
		return block == null;
	}

	@Override
	public int endOfConstruct() {
		return getEnd();
	}

	@Override
	public void setNextElse(ElifConstruct nextElse) {
		throw new UnsupportedOperationException("An else cannot be followed by another elif/else.");
	}

}
