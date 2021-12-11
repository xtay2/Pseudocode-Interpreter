package expressions.normal.array;

import datatypes.ArrayValue;
import datatypes.Castable;
import expressions.normal.Name;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import interpreter.VarManager;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueHolder {

	public final ValueHolder index;
	public final Name name;

	public ArrayAccess(int line, Name array, ValueHolder index) {
		super(line);
		this.name = array;
		this.index = index;
	}

	@Override
	public Castable getValue() {
		return ((ArrayValue) VarManager.get(name.getName()).getValue()).get((int) index.getValue().asInt().rawInt());
	}
	
	public void setValue(Castable var) {
		((ArrayValue) VarManager.get(name.getName()).getValue()).set((int) index.getValue().asInt().rawInt(), var);
	}
}
