package building.types.specific;

import static building.types.specific.operators.InfixOpType.*;

import building.types.AbstractType;
import building.types.SuperType;
import building.types.specific.operators.InfixOpType;

public enum AssignmentType implements AbstractType {

	NORMAL("=", null), ADDI("+=", ADD), SUBI("-=", SUB), MULTI("*=", MULT), DIVI("/=", DIV), POWI("^=", POW), MODI("%=", MOD);

	final String label;

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
		return AbstractType.valHolderTypes();
	}

	@Override
	public String toString() {
		return label;
	}
}
