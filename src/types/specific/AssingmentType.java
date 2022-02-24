package types.specific;

import static types.specific.operators.InfixOpType.*;

import expressions.possible.assigning.Assignment;
import types.AbstractType;
import types.SuperType;
import types.specific.operators.InfixOpType;

public enum AssingmentType implements AbstractType {

	NORMAL("=", null), ADDI("+=", ADD), SUBI("-=", SUB), MULTI("*=", MULT), DIVI("/=", DIV), POWI("^=", POW), MODI("%=", MOD);

	public final String label;

	/** The corresponding Infix-Operator. */
	public final InfixOpType op;

	private AssingmentType(String label, InfixOpType op) {
		this.label = label;
		this.op = op;
	}

	@Override
	public Assignment create(String arg, int lineID) {
		for (AssingmentType t : values()) {
			if (t.label.equals(arg))
				return new Assignment(lineID, t);
		}
		return null;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.ASSIGNMENT_TYPE;
	}
}
