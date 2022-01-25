package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.OPEN_SCOPE;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.normal.Expression;
import expressions.normal.brackets.OpenScope;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.VarManager;

public class ElseStatement extends Scope implements ElifConstruct {

	public ElseStatement(int line) {
		super(line);
		setExpectedExpressions(OPEN_SCOPE);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 1)
			throw new AssertionError("Merge on an else-statement has to contain an opened scope.");
		block = (OpenScope) e[0];
	}
	
	@Override
	public int endOfConstruct() {
		return getEnd();
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing Else-Statement.");
		VarManager.registerScope(this);
		if (!callNextLine()) {
			VarManager.deleteScope(this);
			return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
		}
		VarManager.deleteScope(this);
		return true; // Lasse das if-Statement die nächste Zeile ausführen.
	}

	@Override
	public String getScopeName() {
		return "else" + getStart() + "-" + getEnd();
	}

	@Override
	public void setNextElse(ElifConstruct nextElse) {
		throw new IllegalCodeFormatException(getOriginalLine(), "An else cannot be followed by another elif/else.");
	}
}
