package org.jpl7;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.jpl7.fli.DoubleHolder;
import org.jpl7.fli.Int64Holder;
import org.jpl7.fli.IntHolder;
import org.jpl7.fli.Prolog;
import org.jpl7.fli.StringHolder;
import org.jpl7.fli.term_t;

/**
 * Term is the abstract base class for Compound, Atom, Variable, Integer and Float, which comprise a Java-oriented concrete syntax for Prolog. You cannot create instances of Term directly; rather, you
 * should create instances of Term's concrete subclasses. Alternatively, use textToTerm() to construct a Term from its conventional Prolog source text representation.
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
 * @author Fred Dushin <fadushin@syr.edu>
 * @version $Revision$
 */
public abstract class Term {

	/**
	 * This default constructor enables subclasses to define their own default constructors
	 */
	protected Term() {
	}

	/**
	 * returns the i-th (1+) argument of a Term; defined only for Compound (and hence Atom); throws a JPLException for other Term subclasses
	 * 
	 * @return the i-th argument of a (Compound) Term
	 */
	public Term arg(int i) { // overridden in Compound
		throw new JPLException("arg(int) is undefined for " + this.typeName() + " instances");
	}

	/**
	 * returns, as a Term[], the arguments of a Compound returns an empty Term[] from an Atom, Integer or Float throws a JPLException from a Variable
	 * 
	 * @return the arguments of a Compound as a Term[
	 */
	public Term[] args() { // overridden in Compound
		return new Term[] {};
	}

	public String atomType() { // overridden in Atom
		return null;
	}

	/**
	 * Tests whether this Term's functor has (String) 'name' and 'arity' Returns false if called inappropriately
	 * 
	 * @return whether this Term's functor has (String) 'name' and 'arity'
	 */
	public boolean hasFunctor(String name, int arity) { // overridden in Compound
		return false;
	}

	/**
	 * Tests whether this Term's functor has (int) 'name' and 'arity' Returns false if called inappropriately
	 * 
	 * @return whether this Term's functor has (int) 'name' and 'arity'
	 */
	public boolean hasFunctor(int value, int arity) {
		return false;
	}

	/**
	 * Tests whether this Term's functor has (double) 'name' and 'arity' Returns false if called inappropriately
	 * 
	 * @return whether this Term's functor has (double) 'name' and 'arity'
	 */
	public boolean hasFunctor(double value, int arity) {
		return false;
	}

	/**
	 * returns, as a String, the name of a Compound, Atom or Variable throws a JPLException from an Integer or Float
	 * 
	 * @return the name of a Compound, Atom or Variable
	 */
	public String name() { // overridden in Compound, Variable
		throw new JPLException("name() is undefined for " + this.typeName());
	};

	/**
	 * returns, as an int, the arity of a Compound, Atom, Integer or Float; throws a JPLException from a Variable
	 * 
	 * @return the arity of a Compound, Atom, Integer or Float
	 */
	public int arity() { // overridden in Compound, Variable
		return 0;
	};

	/**
	 * returns the value (as an int) of an Integer or Float throws a JPLException from a Compound, Atom or Variable
	 * 
	 * @return the value (as an int) of an Integer or Float
	 */
	public int intValue() {
		throw new JPLException("intValue() is undefined for " + this.typeName());
	}

	/**
	 * returns the value (as a long) of an Integer or Float throws a JPLException from a Compound, Atom or Variable
	 * 
	 * @return the value (as a long) of an Integer or Float
	 */
	public long longValue() {
		throw new JPLException("longValue() is undefined for " + this.typeName());
	}

	/**
	 * returns the value (as a float) of an Integer or Float throws a JPLException from a Compound, Atom or Variable
	 * 
	 * @return the value (as a float) of an Integer or Float
	 */
	public float floatValue() {
		throw new JPLException("floatValue() is undefined for " + this.typeName());
	}

	/**
	 * returns the value (as a java.math.BigInteger) of an Integer; throws a JPLException from a Compound, Atom, Float or Variable
	 * 
	 * @return the value (as a java.math.BigInteger) of an Integer; throws a JPLException from a Compound, Atom, Float or Variable
	 */
	public BigInteger bigValue() {
		throw new JPLException("bigValue() is undefined for " + this.typeName());
	}

	/**
	 * returns the value (as a double) of an Integer or Float throws a JPLException from any other subclass
	 * 
	 * @return the value (as an double) of an Integer or Float
	 */
	public double doubleValue() {
		throw new JPLException("doubleValue() is undefined for " + this.typeName());
	}

	/**
	 * returns the type of this term, as one of org.jpl7.fli.Prolog.COMPOUND, .ATOM, .VARIABLE, .INTEGER, .FLOAT etc
	 * 
	 * @return the type of this term, as one of org.jpl7.fli.Prolog.COMPOUND, .ATOM, .VARIABLE, .INTEGER, .FLOAT etc
	 */
	public abstract int type();

	/**
	 * returns the name of the type of this term, as one of "Compound", "Atom", "Variable", "Integer", "Float" etc
	 * 
	 * @return the name of the type of this term, as one of "Compound", "Atom", "Variable", "Integer", "Float" etc
	 */
	public abstract String typeName();

	/**
	 * whether this Term is an Atom (of some kind)
	 * 
	 * @return whether this Term is an Atom (of some kind)
	 */
	public boolean isAtom() {
		return this instanceof Atom;
	}

	/**
	 * whether this Term is an Integer whose value is too big to represent as a long
	 * 
	 * @return whether this Term is an Integer whose value is too big to represent as a long
	 */
	public boolean isBigInteger() {
		return this instanceof Integer && ((Integer) this).isBig();
	}

	/**
	 * whether this Term represents a compound term
	 * 
	 * @return whether this Term represents a compound atom
	 */
	public boolean isCompound() {
		return this instanceof Compound;
	}

	/**
	 * whether this Term represents a float
	 * 
	 * @return whether this Term represents a float
	 */
	public boolean isFloat() {
		return this instanceof Float;
	}

	/**
	 * whether this Term represents an integer
	 * 
	 * @return whether this Term represents an integer
	 */
	public boolean isInteger() {
		return this instanceof Integer;
	}

	/**
	 * whether this Term is a variable
	 * 
	 * @return whether this Term is a variable
	 */
	public boolean isVariable() {
		return this instanceof Variable;
	}

	/**
	 * whether this Term denotes (syntax-specifically) an empty list
	 * 
	 * @return whether this Term denotes (syntax-specifically) an empty list
	 */
	public boolean isListNil() { // overridden in Atom
		return false;
	}

	/**
	 * whether this Term denotes (syntax-specifically) a list pair
	 * 
	 * @return whether this Term denotes (syntax-specifically) a list pair
	 */
	public boolean isListPair() { // overridden in Compound
		return false;
	}

	/**
	 * whether this Term is a 'jfalse' structure, i.e. @(false)
	 * 
	 * @return whether this Term is a 'jfalse' structure, i.e. @(false)
	 */
	public boolean isJFalse() {
		return false; // overridden in Compound, where it might sometimes be true
	}

	/**
	 * whether this Term is a 'jnull' structure, i.e. @(null)
	 * 
	 * @return whether this Term is a 'jnull' structure, i.e. @(null)
	 */
	public boolean isJNull() {
		return false; // overridden in Compound, where it might sometimes be true
	}

	/**
	 * whether this Term is a 'jobject' structure, i.e. @(Tag)
	 * 
	 * @return whether this Term is a 'jobject' structure, i.e. @(Tag)
	 */
	public boolean isJObject() {
		return false; // overridden in Compound, where it might sometimes be true
	}

	/**
	 * whether this Term is a 'jref' structure, i.e. @(Tag) or @(null)
	 * 
	 * @return whether this Term is a 'jref' structure, i.e. @(Tag) or @(null)
	 */
	public boolean isJRef() {
		return false; // overridden in Compound, where it might sometimes be true
	}

	/**
	 * whether this Term is a 'jtrue' structure, i.e. @(true)
	 * 
	 * @return whether this Term is a 'jtrue' structure, i.e. @(true)
	 */
	public boolean isJTrue() {
		return false; // overridden in Compound, where it might sometimes be true
	}

	/**
	 * whether this Term is a 'jvoid' structure, i.e. @(void)
	 * 
	 * @return whether this Term is a 'jvoid' structure, i.e. @(void)
	 */
	public boolean isJVoid() {
		return false; // overridden in Compound, where it might sometimes be true
	}

	// public abstract Object jrefToObject();
	public Object jrefToObject() {
		throw new JPLException("term is not a JRef");
	}

	/**
	 * returns a new Term instance which represents the given object
	 */
	public static Term objectToJRef(Object obj) {
		return new Compound("@", new Term[] { new Atom(Prolog.object_to_tag(obj)) });
	}

	public Term putParams(Term[] ps) { // necessarily (?) public
		IntHolder next = new IntHolder();
		next.value = 0;
		Term t2 = this.putParams1(next, ps);
		if (next.value != ps.length) {
			throw new JPLException("more actual params than formal");
		}
		return t2;
	}

	protected Term putParams(Term plist) { // was public
		Term[] ps = plist.toTermArray();
		return putParams(ps);
	}

	protected Term putParams1(IntHolder next, Term[] ps) {
		switch (this.type()) {
		case Prolog.COMPOUND:
			return new Compound(this.name(), putParams2(this.args(), next, ps));
		case Prolog.ATOM:
			if (!this.name().equals("?")) {
				return this;
			} else if (next.value >= ps.length) {
				throw new JPLException("fewer actual params than formal params");
			} else {
				return ps[next.value++];
			}
		default:
			return this;
		}
	}

	static protected Term[] putParams2(Term[] ts, IntHolder next, Term[] ps) {
		int n = ts.length;
		Term[] ts2 = new Term[n];
		for (int i = 0; i < n; i++) {
			ts2[i] = ts[i].putParams1(next, ps);
		}
		return ts2;
	}

	/**
	 * the length of this list, iff it is one, else an exception is thrown
	 * 
	 * @throws JPLException
	 * @return the length (as an int) of this list, iff it is one
	 */
	public int listLength() {
		if (this.isListPair()) { // was .hasFunctor(".", 2)
			return 1 + this.arg(2).listLength(); // TODO eliminate recursion
		} else if (this.isListNil()) { // was .hasFunctor("[]", 0)
			return 0;
		} else {
			throw new JPLException("term is not a list");
		}
	}

	/**
	 * returns an array of terms which are the successive members of this list, if it is a list, else throws an exception
	 * 
	 * @throws JPLException
	 * @return an array of terms which are the successive members of this list, if it is a list
	 */
	public Term[] toTermArray() {
		try {
			int len = this.listLength(); // exception if not a well formed list
			Term[] ts = new Term[len];
			Term t = this;
			for (int i = 0; i < len; i++) { // no need to check functor (listLength did)
				ts[i] = t.arg(1);
				t = t.arg(2);
			}
			return ts;
		} catch (JPLException e) {
			throw new JPLException("term is not a proper list");
		}
	}

	// /**
	// * Returns a debug-friendly representation of a Term
	// *
	// * @return a debug-friendly representation of a Term
	// * @deprecated
	// */
	// public abstract String debugString();

	// /**
	// * Returns a debug-friendly representation of a list of Terms
	// *
	// * @return a debug-friendly representation of a list of Terms
	// * @deprecated
	// */
	// public static String debugString(Term arg[]) {
	// String s = "[";
	// for (int i = 0; i < arg.length; ++i) {
	// s += arg[i].debugString();
	// if (i != arg.length - 1) {
	// s += ", ";
	// }
	// }
	// return s + "]";
	// }

	// ==================================================================/
	// Converting JPL Terms to Prolog terms
	//
	// To convert a Term to a term_t, we need to traverse the Term
	// structure and build a corresponding Prolog term_t object.
	// There are some issues:
	//
	// - Prolog term_ts rely on the *consecutive* nature of term_t
	// references. In particular, to build a compound structure
	// in the Prolog FLI, one must *first* determine the arity of the
	// compound, create a *sequence* of term_t references, and then
	// put atoms, functors, etc. into those term references. We
	// do this in these methods by first determining the arity of the
	// Compound, and then by "put"-ing a type into a term_t.
	// The "put" method is implemented differently in each of Term's
	// five subclasses.
	//
	// - What if we are trying to make a term_t from a Term, but the
	// Term has several same-named Variables? We want
	// to ensure that one Prolog variable will be created and shared, or else
	// queries will give incorrect answers. We thus pass a Map
	// (var_table) through these methods. The table contains term_t
	// instances, keyed on Variable names.
	// ==================================================================/

	protected void put(term_t term) { // was public
		put(new HashMap<String, term_t>(), term);
	}

	/**
	 * Cache the reference to the Prolog term_t here.
	 * 
	 * @param varnames_to_vars
	 *            A Map from variable names to JPL Variables.
	 * @param term
	 *            A (previously created) term_t which is to be put with a Prolog term-type appropriate to the Term type (e.g., Atom, Variable, Compound, etc.) on which the method is invoked.)
	 */
	protected abstract void put(Map<String, term_t> varnames_to_vars, term_t term);

	/**
	 * This static method converts an array of Terms to a *consecutive* sequence of term_t objects. Note that the first term_t object returned is a term_t class (structure); the succeeding term_t
	 * objects are consecutive references obtained by incrementing the *value* field of the term_t.
	 * 
	 * @param varnames_to_vars
	 *            Map from variable names to JPL Variables.
	 * @param args
	 *            An array of org.jpl7.Term references.
	 * @return consecutive term_t references (first of which is a structure)
	 */
	protected static term_t putTerms(Map<String, term_t> varnames_to_vars, Term[] args) {
		// First create a sequence of term_ts. The 0th term_t will be a org.jpl7.fli.term_t.
		// Successive Prolog term_t references will reside in the Prolog engine, and can be obtained by term0.value+i.
		term_t term0 = Prolog.new_term_refs(args.length);
		// for each new term reference, construct a Prolog term by putting an appropriate Prolog type into the reference.
		long ith_term_t = term0.value;
		for (int i = 0; i < args.length; ++i, ++ith_term_t) {
			term_t term = new term_t();
			term.value = ith_term_t;
			args[i].put(varnames_to_vars, term); // each subclass defines its own put()
		}
		return term0;
	}

	// experiment: for jni_jobject_to_term_byval/2 in jpl.c
	public static void putTerm(Object obj, term_t termref) {
		if (obj instanceof Term) {
			((Term) obj).put(termref);
		} else {
			throw new JPLException("not a Term");
		}
	}

	/**
	 * This method calls from_term_t on each term in the n consecutive term_ts. A temporary term_t "holder" (byref) structure must be created in order to extract type information from the Prolog
	 * engine.
	 * 
	 * @param vars_to_Vars
	 *            A Map from Prolog variables to org.jpl7.Variable instances
	 * @param n
	 *            The number of consecutive term_ts
	 * @param term0
	 *            The 0th term_t (structure); subsequent term_ts are not structures.
	 * @return An array of converted Terms
	 */
	/*
	 * protected static Term[] from_term_ts(Map vars_to_Vars, int n, term_t term0) {
	 * 
	 * // create an (uninitialised) array of n Term references Term[] terms = new Term[n];
	 * 
	 * // for each term_t (from 0...n-1), create a term_t // (temporary) structure and dispatch the translation // to a Term to the static from_term_t method of the Term // class. This will perform
	 * (Prolog) type analysis on the // term_t and call the appropriate static method to create // a Term of the right type (e.g., Atom, Variable, List, etc.) // long ith_term_t = term0.value; for
	 * (int i = 0; i < n; ++i, ++ith_term_t) { term_t term = new term_t(); term.value = ith_term_t;
	 * 
	 * terms[i] = Term.from_term_t(vars_to_Vars, term); }
	 * 
	 * return terms; }
	 */

	/**
	 * create and return a org.jpl7.Term representation of the given Prolog term
	 * 
	 * @param vars_to_Vars
	 *            A Map from Prolog variables to org.jpl7.Variable instances
	 * @param term
	 *            The Prolog term (in a term_t holder) to convert
	 * @return The converted Term subclass instance.
	 */
	protected static Term getTerm(Map<term_t, Variable> vars_to_Vars, term_t term) {
		StringHolder hString;
		IntHolder hInt;
		Int64Holder hInt64;
		switch (Prolog.term_type(term)) {
		case Prolog.VARIABLE:
			for (Iterator<term_t> i = vars_to_Vars.keySet().iterator(); i.hasNext();) {
				term_t varX = (term_t) i.next(); // a previously seen Prolog variable
				if (Prolog.compare(varX, term) == 0) { // identical Prolog variables?
					return (Term) vars_to_Vars.get(varX); // return the associated JPL Variable
				}
			}
			// otherwise, the Prolog variable in term has not been seen before
			Variable Var = new Variable(); // allocate a new (sequentially named) Variable to represent it
			Var.term_ = term; // this should become redundant...
			vars_to_Vars.put(term, Var); // use Hashtable(var,null), but only need set(var)
			return Var;
		case Prolog.ATOM:
			hString = new StringHolder();
			Prolog.get_atom_chars(term, hString); // ignore return val; assume success...
			return new Atom(hString.value, "text");
		case Prolog.STRING:
			hString = new StringHolder();
			Prolog.get_string_chars(term, hString); // ignore return val; assume success...
			return new Atom(hString.value, "string");
		case Prolog.INTEGER:
			hInt64 = new Int64Holder();
			if (Prolog.get_integer(term, hInt64)) { // assume it fails if Prolog integer is bigger than a Java long...
				return new org.jpl7.Integer(hInt64.value);
			} else {
				hString = new StringHolder();
				if (Prolog.get_integer_big(term, hString)) {
//					System.out.println("bigint = " + hString.value);
					return new org.jpl7.Integer(new java.math.BigInteger(hString.value));
				} else {
					return new org.jpl7.Integer(-3); // arbitrary
				}
			}
		case Prolog.FLOAT:
			DoubleHolder hFloatValue = new DoubleHolder();
			Prolog.get_float(term, hFloatValue); // assume it succeeds...
			return new org.jpl7.Float(hFloatValue.value);
		case Prolog.COMPOUND:
		case Prolog.LIST_PAIR:
			hString = new StringHolder();
			hInt = new IntHolder();
			Prolog.get_name_arity(term, hString, hInt); // assume it succeeds
			Term args[] = new Term[hInt.value];
			// term_t term1 = Prolog.new_term_refs(hArity.value);
			for (int i = 1; i <= hInt.value; i++) {
				term_t termi = Prolog.new_term_ref();
				Prolog.get_arg(i, term, termi);
				args[i - 1] = Term.getTerm(vars_to_Vars, termi);
			}
			return new Compound(hString.value, args);
		case Prolog.LIST_NIL:
			return JPL.LIST_NIL;
		default:
			throw new JPLException("unknown term type=" + Prolog.term_type(term)); // should never happen...
		}
	}

	protected static Term getTerm(term_t term) {
		return getTerm(new HashMap<term_t, Variable>(), term);
	}

	// ==================================================================/
	// Computing Substitutions
	//
	// Once a solution has been found, the Prolog term_t references
	// will have been instantiated and will refer to new terms. To compute
	// a substitution, we traverse the (original) Term structure, looking
	// at the term_t reference in the Term. The only case we really care
	// about is if the (original) Term is a Variable; if so, the term_t
	// back in the Prolog engine may be instantiated (non Variable parts
	// of the original Term cannot change or become uninstantiated). In
	// this case, we can store this term in a Map, keyed by the
	// Variable with which the term was unified.
	// ==================================================================/

	/**
	 * This method computes a substitution from a Term. The bindings Map stores Terms, keyed by names of Variables. Thus, a substitution is as it is in mathematical logic, a sequence of the form
	 * \sigma = {t_0/x_0, ..., t_n/x_n}. Once the substitution is computed, the substitution should satisfy
	 * 
	 * \sigma T = t
	 * 
	 * where T is the Term from which the substitution is computed, and t is the term_t which results from the Prolog query.
	 * <p>
	 * 
	 * A second Map, vars, is required; this table holds the Variables that occur (thus far) in the unified term. The Variable instances in this table are guaranteed to be unique and are keyed on
	 * Strings which are Prolog internal representations of the variables.
	 * 
	 * @param bindings
	 *            table holding Term substitutions, keyed on names of Variables.
	 * @param vars
	 *            A Map holding the Variables that occur thus far in the term; keyed by internal (Prolog) string rep.
	 */
	protected void getSubst(Map<String, Term> varnames_to_Terms, Map<term_t, Variable> vars_to_Vars) { // overridden in Compound, Variable
	}

	/**
	 * Just calls computeSubstitution for each Term in the array.
	 * 
	 * @param varnames_to_Terms
	 *            a Map from variable names to Terms
	 * @param vars_to_Vars
	 *            a Map from Prolog variables to JPL Variables
	 * @param args
	 *            an array of Terms
	 */
	protected static void getSubsts(Map<String, Term> varnames_to_Terms, Map<term_t, Variable> vars_to_Vars, Term[] args) {
		for (int i = 0; i < args.length; ++i) {
			args[i].getSubst(varnames_to_Terms, vars_to_Vars);
		}
	}

	/**
	 * This method is used (by Compound.equals) to determine the Terms in two Term arrays are pairwise equal, where two Terms are equal if they satisfy the equals predicate (defined differently in
	 * each Term subclass).
	 * 
	 * @param t1
	 *            an array of Terms
	 * @param t2
	 *            another array of Terms
	 * @return true if all of the Terms in the (same-length) arrays are pairwise equal
	 */
	protected static boolean terms_equals(Term[] t1, Term[] t2) {
		if (t1.length != t2.length) {
			return false;
		} else {
			for (int i = 0; i < t1.length; ++i) {
				if (!t1[i].equals(t2[i])) {
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * Converts a list of Terms to a String.
	 * 
	 * @param args
	 *            An array of Terms to convert
	 * @return String representation of a list of Terms
	 */
	public static String toString(Term[] args) {
		String s = "";
		for (int i = 0; i < args.length; ++i) {
			s += args[i].toString();
			if (i != args.length - 1) {
				s += ", ";
			}
		}
		return s;
	}

}
