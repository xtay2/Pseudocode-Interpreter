package modules.merger.sub;

import static types.SuperType.EXPECTED_TYPE;
import static types.specific.BuilderType.COMMA;
import static types.specific.BuilderType.EXPECTED_RETURN_TYPE;
import static types.specific.data.DataType.VAR;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.functions.NativeFunction;
import expressions.main.functions.Returnable;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import modules.merger.SuperMerger;
import types.specific.data.ExpectedType;

public abstract class FuncMerger extends SuperMerger {

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	public static Returnable buildFunc(boolean isNative) {
		line.remove(0);
		Name name = buildName();
		line.remove(0); // OpenBrack
		// PARAMETERS
		if (isNative) {
			List<ExpectedType> params = new ArrayList<>();
			while (line.get(0).type instanceof ExpectedType || line.get(0).is(COMMA)) {
				if (line.get(0).type instanceof ExpectedType)
					((ArrayList<ExpectedType>) params).add(buildExpType());
				else
					line.remove(0);
			}
			line.remove(0); // Closebrack
			return new NativeFunction(lineID, name, params, buildReturnType());
		} else {
			LinkedHashMap<Name, ExpectedType> params = new LinkedHashMap<>();
			do {
				ExpectedType pT = null;
				if (line.get(0).is(EXPECTED_TYPE))
					pT = buildExpType();
				else
					pT = VAR;
				params.put(buildName(), pT);
			} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack

			return new Function(lineID, name, params, buildReturnType(), (OpenScope) build());
		}
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
		return new MainFunction(lineID, (OpenScope) build());
	}
}
