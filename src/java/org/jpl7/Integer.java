package org.jpl7;

import java.math.BigInteger;
import java.util.Map;

import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;

/**
 * Integer is a specialised Term with a long field, representing a Prolog integer value.
 * 
 * <pre>
 * Integer i = new Integer(1024);
 * </pre>
 * 
 * Once constructed, the value of an Integer instance cannot be altered. An Integer can be used (and re-used) as an argument of Compounds. Beware confusing jpl.Integer with java.lang.Integer.
 * 
 * <hr>
 * Copyright (C) 2004 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * <ol>
 * <li> Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 * <li> Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 * </ol>
 *
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * <hr>
 * 
 * @see org.jpl7.Term
 * @see org.jpl7.Compound
 */
public class Integer extends Term {
	private static BigInteger BIG_MIN_LONG = BigInteger.valueOf(java.lang.Long.MIN_VALUE);
	private static BigInteger BIG_MAX_LONG = BigInteger.valueOf(java.lang.Long.MAX_VALUE);

	/**
	 * the Integer's immutable long value, iff small enough
	 */
	protected final long value;

	/**
	 * the Integer's immutable BigInteger value, iff too big for a long
	 */
	protected final BigInteger bigValue;

	/**
	 * @param value
	 *            This Integer's intended (long) value
	 */
	public Integer(long value) {
		this.bigValue = null; // value fits in a long
		this.value = value;
	}

	/**
	 * @param value
	 *            This Integer's intended (BigInteger) value
	 */
	public Integer(BigInteger value) {
		if (value == null) {
			throw new NullPointerException();
		} else if (value.compareTo(BIG_MIN_LONG) >= 0 && value.compareTo(BIG_MAX_LONG) <= 0) { // BIG_MIN_LONG =< value =< BIG_MAX_LONG
			this.bigValue = null; // value fits in a long
			this.value = value.longValue();
		} else {
			this.bigValue = value;
			this.value = 0; // undefined, but 0 by convention, iff bigValue != null
		}
	}

	/**
	 * the (zero) arguments of an Integer, as a (zero-length) Term[]
	 * 
	 * @return the (zero) arguments of an Integer, as a (zero-length) Term[]
	 */
	public Term[] args() {
		return new Term[] {};
	}

	/**
	 * Returns the value of this Integer converted to a double
	 * 
	 * @return the value of this Integer converted to a double
	 */
	public final double doubleValue() {
		if (bigValue == null) {
			return value;
		} else {
			return bigValue.doubleValue();
		}
	}

	/**
	 * two Integer instances are equal if they are the same object, or if their values are equal
	 * 
	 * @param obj
	 *            The Object to compare (not necessarily an Integer)
	 * @return true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof Integer && value == ((Integer) obj).value);
	}

	/**
	 * Returns the value of this Integer converted to a float
	 * 
	 * @return the value of this Integer converted to a float
	 */
	public final float floatValue() {
		if (bigValue == null) {
			return value;
		} else {
			return bigValue.floatValue();
		}
	}

	/**
	 * whether this Integer's functor has (int) 'name' and 'arity' (c.f. traditional functor/3)
	 * 
	 * @return whether this Integer's functor has (int) 'name' and 'arity'
	 */
	public final boolean hasFunctor(int val, int arity) {
		return val == this.value && arity == 0;
	}

	/**
	 * Returns the value of this Integer as an int if possible, else throws a JPLException
	 * 
	 * @throws JPLException
	 *             if the value of this Integer is too great to be represented as a Java int
	 * @return the int value of this Integer
	 */
	public final int intValue() {
		if (bigValue != null || value < java.lang.Integer.MIN_VALUE || value > java.lang.Integer.MAX_VALUE) {
			throw new JPLException("cannot represent value as an int");
		} else {
			return (int) value;
		}
	}

	/**
	 * @return whether this Integer's value is too big to represent as a long
	 */
	protected final boolean isBig() {
		return bigValue != null; // always canonical
	}

	public Object jrefToObject() {
		throw new JPLException("term is not a jref");
	}

	/**
	 * Returns the value of this Integer as a long
	 * 
	 * @return the value of this Integer as a long
	 */
	public final long longValue() {
		if (bigValue != null) { // iff value too big for a long (always canonical)
			throw new JPLException("cannot represent value as a long");
		} else {
			return value;
		}
	}

	/**
	 * Returns the value of this Integer as a java.math.BigInteger, whether or not it fits in a long
	 * 
	 * @return the value of this Integer as a java.math.BigInteger, whether or not it fits in a long
	 */
	public final BigInteger bigValue() {
		if (bigValue == null) {
			return new BigInteger(Long.toString(value)); // oddly (?) cannot construct directly from a long
		} else {
			return bigValue;
		}
	}

	/**
	 * To convert an Integer into a Prolog term, we put its value into the term_t.
	 * 
	 * @param varnames_to_vars
	 *            A Map from variable names to Prolog variables.
	 * @param term
	 *            A (previously created) term_t which is to be set to a Prolog integer
	 */
	protected final void put(Map<String, term_t> varnames_to_vars, term_t term) {
		if (isBig()) {
			Prolog.put_integer_big(term, bigValue.toString());
		} else {
			Prolog.put_integer(term, value);
		}
	}

	/**
	 * a Prolog source text representation of this Integer's value
	 * 
	 * @return a Prolog source text representation of this Integer's value
	 */
	public String toString() {
		if (bigValue == null) {
			return Long.toString(value);
		} else {
			return bigValue.toString(10);
		}
	}

	/**
	 * the type of this term, as "Prolog.INTEGER"
	 * 
	 * @return the type of this term, as "Prolog.INTEGER"
	 */
	public final int type() {
		return Prolog.INTEGER;
	}

	/**
	 * the name of the type of this term, as "Integer"
	 * 
	 * @return the name of the type of this term, as "Integer"
	 */
	public String typeName() {
		return "Integer";
	}

	// /**
	// * Returns the int value of this jpl.Integer
	// *
	// * @return the Integer's value
	// * @deprecated use one of floatValue(), intValue(), longValue()
	// */
	// public final int value() {
	// return (int) value;
	// }

	// /**
	// * Returns a debug-friendly representation of this Integer's value
	// *
	// * @return a debug-friendly representation of this Integer's value
	// * @deprecated
	// */
	// public String debugString() {
	// return "(Integer " + toString() + ")";
	// }

}
