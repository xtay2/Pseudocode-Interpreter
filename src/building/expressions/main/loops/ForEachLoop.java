package building.expressions.main.loops;

import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;
import errorhandeling.*;
import misc.constants.*;
import misc.helper.*;
import runtime.datatypes.array.*;
import runtime.datatypes.numerical.*;

public class ForEachLoop extends Loop {
	
	/** The {@link ValueHolder} that gets called at the start of every new iteration. */
	private final ValueHolder arrayHolder;
	
	/** The temporary value that gets reset for every iteration. */
	private ArrayValue array;
	
	/** The name for the running element. */
	private Name elemName;
	
	/**
	 * Creates a {@link ForEachLoop}.
	 *
	 * @param elementName is the {@link Name} of the running element. Shouldn't be null.
	 * @param arrayH is the Wrapper for the {@link ArrayValue} that later gets iterated over. Shouldn't
	 * be null.
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null.
	 */
	public ForEachLoop(int lineID, Name elemName, ValueHolder arrayH, OpenBlock os) {
		super(lineID, KeywordType.FOR, null, os);
		assert elemName != null : "ElemName cannot be null.";
		assert arrayH != null : "ArrayHolder cannot be null.";
		this.elemName = elemName;
		this.arrayHolder = arrayH;
	}
	
	@Override
	protected void initLoop() {
		super.initLoop();
		try {
			array = (ArrayValue) arrayHolder.as(TypeConstants.VAR_ARR);
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
	}
	
	@Override
	@SuppressWarnings("unlikely-arg-type")
	protected boolean doContinue(NumberValue iteration) {
		try {
			if (iteration.equals(array.length()))
				return false;
			new Variable(lineIdentifier, TypeConstants.VAR, elemName, array.get(MathHelper.valToInt(iteration)));
			return true;
		} catch (ArithmeticException | NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
	}
}
