package interpreting.modules.merger;

import static building.types.SuperType.EXPECTED_TYPE;
import static building.types.specific.BuilderType.CLOSE_BRACKET;
import static building.types.specific.BuilderType.COMMA;
import static building.types.specific.BuilderType.EXPECTED_RETURN_TYPE;
import static building.types.specific.data.DataType.VAR;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import building.expressions.main.functions.Function;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.NativeFunction;
import building.expressions.main.functions.Definition;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.data.ExpectedType;

public abstract class FuncMerger extends SuperMerger {

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	public static Definition buildFunc(boolean isNative) {
		line.remove(0);
		Name name = buildName();
		line.remove(0); // OpenBrack
		// PARAMETERS
		if (isNative)
			return new NativeFunction(lineID, name, buildNativeParams(), buildReturnType());
		else
			return new Function(lineID, name, buildFuncParams(), buildReturnType(), (OpenBlock) build());
	}

	private static List<ExpectedType> buildNativeParams() {
		List<ExpectedType> params = new ArrayList<>();
		while (line.get(0).type instanceof ExpectedType || line.get(0).is(COMMA)) {
			if (line.get(0).type instanceof ExpectedType)
				((ArrayList<ExpectedType>) params).add(buildExpType());
			else
				line.remove(0);
		}
		line.remove(0); // Closebrack
		return params;
	}

	private static LinkedHashMap<Name, ExpectedType> buildFuncParams() {
		LinkedHashMap<Name, ExpectedType> params = new LinkedHashMap<>();
		if (line.get(0).is(CLOSE_BRACKET)) {
			line.remove(0); // Closebrack
			return params;
		}
		do {
			ExpectedType pT = null;
			if (line.get(0).is(EXPECTED_TYPE))
				pT = buildExpType();
			else
				pT = VAR;
			params.put(buildName(), pT);
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		return params;
	}

	/** ([->] [TYPE])? */
	private static ExpectedType buildReturnType() {
		if (!line.isEmpty() && line.get(0).is(EXPECTED_RETURN_TYPE)) {
			line.remove(0); // Arrow
			return buildExpType();
		}
		return null;
	}

	/** [MAIN] [{] */
	public static MainFunction buildMain() {
		line.remove(0);
		return new MainFunction(lineID, (OpenBlock) build());
	}
}
