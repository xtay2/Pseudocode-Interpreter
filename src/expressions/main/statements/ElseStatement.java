package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.OPEN_SCOPE;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.Scope;
import expressions.abstractions.ValueHolder;
import expressions.normal.brackets.OpenScope;
import interpreter.VarManager;
import parsing.program.KeywordType;

public class ElseStatement extends Scope implements ElifConstruct {

	public ElseStatement(int line) {
		super(line, KeywordType.ELSE);
		setExpectedExpressions(OPEN_SCOPE);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 1 || e[0] == null)
			throw new AssertionError("Merge on an else-statement has to contain an opened scope.");
		openScope = (OpenScope) e[0];
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
