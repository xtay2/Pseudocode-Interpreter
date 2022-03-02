package types.specific;

import static types.specific.operators.InfixOpType.*;

import types.AbstractType;
import types.SuperType;
import types.specific.operators.InfixOpType;

public enum AssignmentType implements AbstractType {

	NORMAL("=", null), ADDI("+=", ADD), SUBI("-=", SUB), MULTI("*=", MULT), DIVI("/=", DIV), POWI("^=", POW), MODI("%=", MOD);

	public final String label;

	/** The corresponding Infix-Operator. */
	public final InfixOpType op;

	private AssignmentType(String label, InfixOpType op) {
		this.label = label;
		this.op = op;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.ASSIGNMENT_TYPE;
	}

	@Override
	public AbstractType[] expected() {
		return VAL_HOLDER_TYPES;
	}

	@Override
	public String toString() {
		return label;
	}
}
