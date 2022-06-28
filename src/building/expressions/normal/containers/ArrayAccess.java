package building.expressions.normal.containers;

import static building.types.abstractions.SpecificType.*;
import static building.types.specific.FlagType.*;

import java.util.*;
import java.util.stream.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.normal.containers.name.*;
import building.expressions.possible.multicall.*;
import errorhandeling.*;
import misc.constants.*;
import misc.helper.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements MultiCallableValueChanger {
	
	private final List<ValueHolder> indices;
	private final Name name;
	
	public ArrayAccess(int lineID, Name name, List<ValueHolder> indices) {
		super(lineID, MERGED);
		this.name = name;
		this.indices = indices;
		assert name != null && indices != null : "Name and indices cannot be null.";
		if (indices.isEmpty())
			throw new PseudocodeException("MissingIndex", "An array-access has to hold atleast one index.", getBlueprintPath());
	}
	
	@Override
	public Value getValue() {
		if (indices.size() == 1 && indices.get(0) instanceof MultiCall mc)
			return executeFor(mc.content);
		try {
			return getValue(indices);
		} catch (ArrayIndexOutOfBoundsException iobe) {
			throw new PseudocodeException("ArrayAccess",
					"Index " + indices.stream().map(e -> e.getValue().toString()).collect(Collectors.joining(", ")) + " is out of bounds.",
					getBlueprintPath());
		} catch (NullPointerException npe) {
			throw new AssertionError("Array " + name + " is unitialised.");
		}
	}
	
	private Value getValue(List<ValueHolder> idxs) {
		try {
			Value v = name.as(TypeConstants.VAR_ARR);
			for (ValueHolder index : idxs)
				v = ((ArrayValue) v).get(MathHelper.valToInt(index));
			return v;
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
	}
	
	@Override
	public Value executeFor(ValueHolder[] content) {
		Value[] res = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = getValue(List.of(content[i]));
		return ArrayValue.newInstance(res);
	}
	
	@Override
	public Value setValue(Value val) {
		Variable var = ScopeManager.getVar(name);
		if (var.hasFlag(CONSTANT))
			throw new PseudocodeException("ConstantModification",
					"The Array \"" + name.getNameString() + "\" is defined as constant and cannot be changed.", getBlueprintPath());
		if (indices.size() == 1 && indices.get(0) instanceof MultiCall mc)
			return writeFor(val, mc.content);
		try {
			return ((ArrayValue) var.getValue()).set(val, var.getNameString(), getBlueprintPath(),
					indices.toArray(new ValueHolder[indices.size()]));
		} catch (ClassCastException e) {
			throw new PseudocodeException("InvalidDepth",
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices, getBlueprintPath());
		}
	}
	
	@Override
	public ArrayValue writeFor(Value val, ValueHolder[] content) {
		Variable var = ScopeManager.getVar(name);
		Value[] previous = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			previous[i] = ((ArrayValue) var.getValue()).set(val, var.getNameString(), getBlueprintPath(), content[i]);
		return ArrayValue.newInstance(previous);
	}
	
	@Override
	public Name getName() { return name; }
	
}