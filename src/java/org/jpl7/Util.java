package org.jpl7;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.jpl7.fli.term_t;

/**
 * This class provides a bunch of static utility methods to support JPL's Java API.
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
 * @author Fred Dushin fadushin@syr.edu
 * @version $Revision$
 */
public final class Util {
	/**
	 * Converts an array of Terms to a JPL representation of a Prolog list of terms whose members correspond to the
	 * respective array elements.
	 *
	 * @param terms
	 *            An array of Term
	 * @return Term a list of the array elements
	 * @deprecated Use org.jpl7.Term.termArrayToList(Term[] terms)
	 */
	@Deprecated
	public static Term termArrayToList(Term[] terms) {
		return Term.termArrayToList(terms);
	}

	/**
	 * Converts a substitution, in the form of a Map from variable names to Terms, to a String.
	 *
	 * @param varnames_to_Terms
	 *            A Map from variable names to Terms.
	 * @return String A String representation of the variable bindings
	 */
	public static String subsToString(Map<String, Term> varnames_to_Terms) {
		if (varnames_to_Terms == null) {
			return "[no solution]";
		} else {
			Iterator<String> varnames = varnames_to_Terms.keySet().iterator();
			String s = "Bindings: ";
			while (varnames.hasNext()) {
				String varname = varnames.next();
				s += varname + "=" + varnames_to_Terms.get(varname).toString() + "; ";
			}
			return s;
		}
	}

	/**
	 * @deprecated Use org.jpl7.Util.subsToString(Map<String, Term>)
	 */
	@Deprecated
	public static String toString(Map<String, Term> varnames_to_Terms) {
		return subsToString(varnames_to_Terms);
	}

	/**
	 * Converts a (JPL) list of Name=Var pairs (as yielded by atom_to_term/3) to a Map from Prolog variables
	 * (necessarily in term_t holders) to named JPL Variables
	 *
	 * @param nvs
	 *            A JPL list of Name=Var pairs (as yielded by atom_to_term/3)
	 * @return Map A Map from Prolog variables (necessarily in term_t holders) to named JPL Variables
	 */
	public static Map<term_t, Variable> namevarsToMap(Term nvs) {
		try {
			Map<term_t, Variable> vars_to_Vars = new HashMap<term_t, Variable>();
			// while (nvs.isListPair() && nvs.arg(1).hasFunctor("=", 2)) {
			while (nvs.arity() == 2
					&& (nvs.name().equals(JPL.LIST_PAIR_MODERN) || nvs.name().equals(JPL.LIST_PAIR_TRADITIONAL))
					&& nvs.arg(1).hasFunctor("=", 2)) {
				// the cast to Variable is necessary to access the (protected)
				// .term_ field
				vars_to_Vars.put(((Variable) nvs.arg(1).arg(2)).term_, new Variable(nvs.arg(1).arg(1).name())); // map
				// the
				// Prolog
				// variable
				// to
				// a
				// new,
				// named
				// Variable
				nvs = nvs.arg(2); // advance to next list cell
			}
			// maybe oughta check that nvs is [] ?
			return vars_to_Vars;
		} catch (java.lang.ClassCastException e) { // nvs is not of the expected
			// structure
			return null;
		}
	}

	/**
	 * @deprecated Use org.jpl7.Term.textToTerm(String text)
	 */
	@Deprecated
	public static Term textToTerm(String text) {
		return Term.textToTerm(text);
	}

	/**
	 * Converts a Prolog source text to a corresponding JPL Term (in which each Variable has the appropriate name from
	 * the source text), replacing successive occurrences of ? in the text by the corresponding element of Term[]
	 * params. (New in JPL 3.0.4)
	 *
	 * Throws PrologException containing error(syntax_error(_),_) if text is invalid.
	 *
	 * @param text A Prolog source text denoting a term
	 * @param params parameters to be injected in each ?
	 * @return Term a JPL Term equivalent to the given source text
	 */
	public static Term textParamsToTerm(String text, Term[] params) {
		return Term.textToTerm(text).putParams(params);
	}

	/**
	 * @deprecated Use org.jpl7.Term.stringArrayToList(String[] a)
	 */
	@Deprecated
	public static Term stringArrayToList(String[] a) {
		return Term.stringArrayToList(a);
	}

	/**
	 * @deprecated Use org.jpl7.Term.intArrayToList(int[] a)
	 */
	@Deprecated
	public static Term intArrayToList(int[] a) {
		return Term.intArrayToList(a);
	}

	/**
	 * @deprecated Use org.jpl7.Term.intArrayArrayToList(int[][] a)
	 */
	@Deprecated
	public static Term intArrayArrayToList(int[][] a) {
		return Term.intArrayArrayToList(a);
	}

	/**
	 * @deprecated Use {@link Term#isList(Term)}
	 */
	@Deprecated
	public static final boolean isList(Term term) {
		return Term.isList(term);
	}


	/**
	 * @param term any Term
	 * @return the length of the proper list which the Term represents, else -1
	 * @deprecated Use {@link Term#listLength(Term)}
	 */
	@Deprecated
	public static int listToLength(Term term) {
		return Term.listLength(term);
	}

	/**
	 * @deprecated Use {@link Term#listToTermArray(Term)}
	 */
	@Deprecated
	public static Term[] listToTermArray(Term t) {
		try {
			int len = Term.listLength(t); // exception if not a list
			Term[] ts = new Term[len];
			for (int i = 0; i < len; i++) {
				ts[i] = t.arg(1);
				t = t.arg(2);
			}
			return ts;
		} catch (JPLException e) {
			throw new JPLException("term is not a proper list");
		}
	}

	/**
	 * @deprecated Use {@link Term#atomListToStringArray(Term)}
	 */
	@Deprecated
	public static String[] atomListToStringArray(Term t) {
		int n = Term.listLength(t);
		String[] a;
		if (n < 0) {
			return null;
		} else {
			a = new String[n];
		}
		int i = 0;
		Term head = t;
		while (head.isListPair()) {
			Term x = head.arg(1);
			if (x.isAtom()) {
				a[i++] = x.name();
			} else {
				return null;
			}
			head = head.arg(2);
		}
		return (head.isListNil() ? a : null);
	}
}
