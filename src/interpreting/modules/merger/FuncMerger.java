package interpreting.modules.merger;

import static building.types.abstractions.SuperType.DATA_TYPE;
import static building.types.specific.BuilderType.ARROW_R;
import static building.types.specific.BuilderType.CLOSE_BRACKET;
import static building.types.specific.BuilderType.COMMA;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import building.expressions.main.functions.Definition;
import building.expressions.main.functions.Function;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.NativeFunction;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.abstractions.SuperType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;

public abstract class FuncMerger extends SuperMerger {

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	public static Definition buildFunc(boolean isNative) {
		line.remove(0);
		Name name = buildName();
		line.remove(0); // OpenBrack
		// PARAMETERS
		if (isNative) {
			List<DataType> params = buildNativeParams();
			DataType returnType = buildReturnType();
			return new NativeFunction(lineID, name, params, returnType);
		}
		LinkedHashMap<Name, DataType> params = buildFuncParams();
		DataType returnType = buildReturnType();
		return new Function(lineID, name, params, returnType, (OpenBlock) build());
	}

	/** [->] [TYPE] */
	private static DataType buildReturnType() {
		if (!line.isEmpty() && line.get(0).is(ARROW_R)) {
			line.remove(0);
			return buildExpType();
		}
		return null;
	}

	/* [TYPE]? [,]? ... */
	private static List<DataType> buildNativeParams() {
		List<DataType> params = new ArrayList<>();
		while (line.get(0).type.is(DATA_TYPE)) {
			if (line.get(0).type.is(DATA_TYPE))
				params.add(buildExpType());
			if (line.get(0).type.is(COMMA))
				line.remove(0);
		}
		line.remove(0); // Closebrack
		return params;
	}

	private static LinkedHashMap<Name, DataType> buildFuncParams() {
		LinkedHashMap<Name, DataType> params = new LinkedHashMap<>();
		if (line.get(0).is(CLOSE_BRACKET)) {
			line.remove(0); // Closebrack
			return params;
		}
		do {
			DataType pT = null;
			if (line.get(0).is(SuperType.DATA_TYPE)) {
				pT = buildExpType();
			} else
				pT = new DataType(SingleType.VAR, true);
			params.put(buildName(), pT);
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		return params;
	}

	/** [MAIN] [{] */
	public static MainFunction buildMain() {
		line.remove(0);
		return new MainFunction(lineID, (OpenBlock) build());
	}
}
