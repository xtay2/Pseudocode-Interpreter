package unittests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.jupiter.api.Test;

import exceptions.CastingException;
import programreader.expressions.special.Type;
import programreader.expressions.special.Value;

class Casting {

	@SuppressWarnings("removal")
	@Test
	void fromInt() {
		Value a = new Value(new Integer(10), Type.NUMBER);
		assertTrue(a.asBool());
		assertTrue("10".equals(a.asText()));

		assertTrue(10 == a.asInt());
		assertFalse(11 == a.asInt());

		assertTrue(10.0 == a.asDouble());
		assertFalse(10.3 == a.asDouble());

		assertTrue(a.asNr() instanceof Integer);
	}

	@SuppressWarnings("removal")
	@Test
	void fromDouble() {
		Value a = new Value(new Double(10.5234), Type.NUMBER);
		assertTrue(a.asBool());
		assertTrue("10.5234".equals(a.asText()));

		assertTrue(10 == a.asInt());
		assertFalse(11 == a.asInt());

		assertTrue(10.5234 == a.asDouble());
		assertFalse(10.0 == a.asDouble());

		assertTrue(a.asNr() instanceof Double);
		
		Value b = new Value(new Double(10.0), Type.NUMBER);
		
		assertTrue(b.asNr() instanceof Integer);
		System.out.println(b.asText());
	}

	@Test
	void fromString() {
		Value a = new Value("Hallo", Type.TEXT);

		assertTrue("Hallo".equals(a.asText()));
		assertFalse("hallo".equals(a.asText()));

		try {
			a.asBool();
			assertFalse("Expected CastingException", true);
		} catch (CastingException e) {
			System.out.println("Exeption caught.");
		}

		try {
			a.asInt();
			assertFalse("Expected CastingException", true);
		} catch (CastingException e) {
			System.out.println("Exeption caught.");
		}

		try {
			a.asDouble();
			assertFalse("Expected CastingException", true);
		} catch (CastingException e) {
			System.out.println("Exeption caught.");
		}

		try {
			a.asNr();
			assertFalse("Expected CastingException", true);
		} catch (CastingException e) {
			System.out.println("Exeption caught.");
		}

		Value b = new Value("10.6", Type.TEXT);

		try {
			b.asBool();
			assertFalse("Expected CastingException", true);
		} catch (CastingException e) {
			System.out.println("Exeption caught.");
		}

		assertTrue(b.asInt() == 10);
		assertFalse(b.asInt() == 11);

		assertTrue(b.asDouble() == 10.6);
		assertFalse(b.asDouble() == 11.0);

		assertTrue(b.asNr() instanceof Double);
		assertFalse(b.asNr() instanceof Integer);

		Value c = new Value("0", Type.TEXT);

		assertFalse(c.asBool());
	}

	@Test
	void fromBool() {
		Value a = new Value(true, Type.BOOL);
		assertTrue(a.asBool());
		assertTrue(a.asText().equals("1"));
		assertTrue(a.asInt() == 1);
		assertTrue(a.asDouble() == 1.0);
		assertTrue(a.asNr() instanceof Integer);
	}

}
