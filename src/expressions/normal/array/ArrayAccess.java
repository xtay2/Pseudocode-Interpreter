package expressions.normal.array;

import datatypes.Castable;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import interpreter.VarManager;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueHolder {

	public final ValueHolder index;
	public final String array;

	public ArrayAccess(int line, String array, ValueHolder index) {
		super(line);
		this.array = array;
		this.index = index;
	}

	@Override
	public Castable getValue() {
		return VarManager.get(array).getValue();
	}

}
