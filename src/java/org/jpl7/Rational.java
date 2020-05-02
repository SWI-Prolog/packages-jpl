package org.jpl7;

import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;

import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;

/**
 * Integer is a specialised Term representing a Prolog integer value; if the value fits, it is held in a long field,
 * else as a BigInteger.
 *
 * <pre>
 * Integer i = new Integer(1024);
 * </pre>
 *
 * Once constructed, the value of an Integer instance cannot be altered. An Integer can be used (and re-used) as an
 * argument of Compounds. Beware confusing jpl.Integer with java.lang.Integer.
 *
 * <hr>
 * Copyright (C) 2004 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 * <ol>
 * <li>Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 * disclaimer.
 *
 * <li>Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
 * following disclaimer in the documentation and/or other materials provided with the distribution.
 * </ol>
 *
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * <hr>
 *
 * @see Term
 * @see Compound
 */
public class Rational extends Term {

	/**
	 * the numerator and denominator in canonical form:
	 * 		gcd(numerator, denominator =  1 (not reducable)
	 * 		denominator > 1
	 *
	 * 	rat is a String of the form "NrM"
	 */
	protected final long numerator;
	protected final long denominator;

	/**
	 * Creates a rational in canonical form
	 *
	 * @param numerator		a long integer
	 * @param denominator	a long integer
	 *
	 */
	public Rational(long numerator, long denominator) {
		if (denominator == 0) {
			throw new JPLException("denominator of rational cannot be 0");
		} else {
			// reduce fraction
			long g = gcd(numerator, denominator);

			long num = numerator / g;
			long dem = denominator / g;

			// needed only for negative numbers
			if (dem < 0) {
				this.denominator = -dem;
				this.numerator = -num;
			} else {
				this.denominator = dem;
				this.numerator = num;
			}
		}
		if (denominator == 1) {
			throw new JPLException("the denominator is 1 so it should be an Integer.");
		}
	}

	/**
	 * 	Creates a rational in canonical form
	 *
	 * @param rat   The rational number in format NrM
	 */
	public Rational(String rat) {
		if (Pattern.compile("-?\\d+r\\d+").matcher(rat).matches()) {
			long numerator = Long.parseLong(rat.split("r", -2)[0]);
			long denominator = Long.parseLong(rat.split("r", -2)[1]);

			if (denominator == 0) {
				throw new JPLException("denominator of rational cannot be 0");
			} else {
				// reduce fraction
				long g = gcd(numerator, denominator);

				long num = numerator / g;
				long dem = denominator / g;

				// needed only for negative numbers
				if (dem < 0) {
					this.denominator = -dem;
					this.numerator = -num;
				} else {
					this.denominator = dem;
					this.numerator = num;
				}
				if (denominator == 1) {
					throw new JPLException("the denominator is 1 so it should be an Integer.");
				}
			}
		} else {
			throw new JPLException("incorrect format for rational number; should be of the form NrM.");
		}
	}

	// return gcd(|m|, |n|)
	private static long gcd(long m, long n) {
		if (m < 0) m = -m;
		if (n < 0) n = -n;
		if (0 == n) return m;
		else return gcd(n, m % n);
	}

	public Term[] args() {
		return new Term[] {};
	}

	/**
	 * Two Rationals instances are equal if their numerator and denominator match
	 * (remember Rationals are stored in canonical form)
	 *
	 * @param o
	 *            The Object to compare (not necessarily an Integer)
	 * @return true if the Object satisfies the above condition
	 */
	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		Rational rational = (Rational) o;
		return numerator == rational.numerator &&
				denominator == rational.denominator;
	}

	@Override
	public int hashCode() {
		return Objects.hash(numerator, denominator);
	}





	public final long getNumerator() {
		return numerator;
	};
	public final long getDenominator() {
		return denominator;
	};

	/**
	 * whether this Rational's functor has (long) 'name' and 'arity' (c.f. traditional functor/3)
	 *
	 * @return whether this Rational's functor has (long) 'name' and 'arity'
	 */
	public final boolean hasFunctor(long val, int arity) {
		return this.numerator == val &&  arity == 0;
	}


	/**
	 * Returns the value of this Rational as an int if possible, else throws a JPLException
	 *
	 * @throws JPLException
	 *             if the value of this Rational is too great to be represented as a Java int
	 * @return the int value of this Rational
	 */
	public final int intValue() {
		return Math.round(numerator / denominator);
	}

		/**
	 * Returns the value of this org.jpl7.Rational as a long
	 *
	 * @return the value of this org.jpl7.Rational as a long
	 */
	public final long longValue() {
			return (numerator / denominator);
	}

	/**
	 * Returns the value of this Rational converted to a float
	 *
	 * @return the value of this Rational converted to a float
	 */
	public final float floatValue() {
		return numerator / (float) denominator;
	}

	/**
	 * Returns the value of this Rational converted to a double
	 *
	 * @return the value of this Rational converted to a double
	 */
	public final double doubleValue() {
		return numerator / (double) denominator;
	}




	/**
	 * To convert an Rational into a Prolog term, we put its value into the term_t.
	 *
	 * @param varnames_to_vars
	 *            A Map from variable names to Prolog variables.
	 * @param term
	 *            A (previously created) term_t which is to be set to a Prolog integer
	 */
	protected final void put(Map<String, term_t> varnames_to_vars, term_t term) {
		Prolog.put_rational(term, this.toString());
	}

	/**
	 * a Prolog source text representation of this Rational's value
	 *
	 * @return a Prolog source text representation of this Rational's value
	 */
	public String toString() {
		return String.format("%sr%s", numerator, denominator);
	}

	/**
	 * the type of this term, as "Prolog.INTEGER"
	 *
	 * @return the type of this term, as "Prolog.INTEGER"
	 */
	public final int type() {
		return Prolog.RATIONAL;
	}

	/**
	 * the name of the type of this term, as "Rational"
	 *
	 * @return the name of the type of this term, as "Rational"
	 */
	public String typeName() {
		return "Rational";
	}

}
