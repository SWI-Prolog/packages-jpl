package org.jpl7;

import java.math.BigInteger;
import java.util.Map;
import java.util.Objects;

import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;

/**
 * This class supports Java representations of Prolog variables.
 * <p>
 *
 * A jpl.Variable instance is equivalent to a variable in a fragment of Prolog
 * source text: it is *not* a "live" variable within a Prolog stack or heap. A
 * corresponding Prolog variable is created only upon opening a Query whose goal
 * refers to a Variable (and then only temporarily).
 *
 * <hr>
 * Copyright (C) 2004 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * <ol>
 * <li>Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * <li>Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * </ol>
 *
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * <hr>
 *
 * @author Fred Dushin fadushin@syr.edu
 * @version $Revision$
 */
public class Variable extends Term {
	/**
	 * the integral part of the next automatic variable name to be allocated
	 */
	private static long n = 0;

	protected transient int index; // only used by (redundant?)

	/**
	 * the name of this Variable (may be changed as of April 2020)
	 */
	public  String name;


	/**
	 * defined between Query.open() and Query.get2()
	 */
	protected transient term_t term_ = null;

	/**
	 * Create a new don't tell Variable with new sequential name of the form "_261".
	 *
	 */
	public Variable() {
		this.name = "_" + Long.toString(n++); // e.g. _0, _1 etc.
	}

	/**
	 * Create a new Variable with 'name' (which must not be null or ""), and may
	 * one day be constrained to comply with traditional Prolog syntax.
	 *
	 * @param name
	 *            the source name of this Variable
	 */
	public Variable(String name) {
		if (name == null) {
			throw new JPLException("name cannot be null");
		} else if (name.equals("")) {
			throw new JPLException("name cannot be empty String");
		} else {
			this.name = name;
		}
	}

	public final Term[] args() {
		throw new JPLException("args() is undefined for Variable");
	}

	/**
	 * returns, as an int, the arity of a Term
	 *
	 * @return the arity of a Term
	 */
	public int arity() {
		throw new JPLException("arity() is undefined for Variable");
	};

	/**
	 * A Variable is equal to another if their names are the same
	 * Totally lexical textual interpretation of vars, not related to bindings
	 * Anonymous var _ is always different (including to itsef)
	 *
	 * @param o
	 *            The Object to compare.
	 * @return true if the Object is a Variable and the above condition apply.
	 */
	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof Variable) || this.name.equals("_") || ((Variable) o).name.equals("_")) return false;
		if (this == o) return true;
		Variable variable = (Variable) o;
		return name.equals(variable.name);
	}


	// just use the id which is unique for each object
	@Override
	public int hashCode() {
		return Objects.hash(name);
	}

	/**
	 * This can be used to get the **current** solution binding of the Variable instance.
	 * So we are calling when the goal is in a solution
	 *
	 * This method returns nothing but modifies the argument mappings, by filling
	 * them with the current solution binding of the Variable instance.
	 *
	 * If Variable instance is anonymous or a dont-tell-me variable, then do nothing
	 *
	 * Otherwise, if the Variable instance is not already in the varnames_to_Terms Map,
	 * put the result of converting the term_t to which this variable has been unified
	 * (in the current solution) to a Term in the Map, keyed on this Variable's name.
	 *
	 * In binding the Variable instance to the Term (in the current solution), such Term
	 * may also refer to free variables, whose maps are stores in vars_to_Vars
	 *
	 * @param varnames_to_Terms
	 *            A Map of bindings from variable names to JPL Terms.
	 * @param vars_to_Vars
	 *            A Map from Prolog variables to JPL Variables.
	 */
	protected final void getSubst(Map<String, Term> varnames_to_Terms, Map<term_t, Variable> vars_to_Vars) {
		// NB a Variable.name cannot be "" i.e. of 0 length
		// if (!(this.name.charAt(0) == '_') && varnames_to_Terms.get(this.name)
		// == null) {
		if (tellThem() && varnames_to_Terms.get(this.name) == null) {
			varnames_to_Terms.put(this.name, Term.getTerm(vars_to_Vars, this.term_));
		}
	}

	/* (non-Javadoc)
	 * @see org.jpl7.Term#hasFunctor(java.math.BigInteger, int)
	 */
	public boolean hasFunctor(BigInteger value, int arity) {
		throw new JPLException("hasFunctor() is undefined for Variable");
	}

	/* (non-Javadoc)
	 * @see org.jpl7.Term#hasFunctor(java.lang.String, int)
	 */
	public boolean hasFunctor(String name, int arity) {
		throw new JPLException("hasFunctor() is undefined for Variable");
	}

	/* (non-Javadoc)
	 * @see org.jpl7.Term#hasFunctor(long, int)
	 */
	public boolean hasFunctor(long value, int arity) {
		throw new JPLException("hasFunctor() is undefined for Variable");
	}

	/* (non-Javadoc)
	 * @see org.jpl7.Term#hasFunctor(double, int)
	 */
	public boolean hasFunctor(double value, int arity) {
		throw new JPLException("hasFunctor() is undefined for Variable");
	}

	/**
	 * the lexical name of this Variable
	 *
	 * @return the lexical name of this Variable
	 */
	public final String name() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	/**
	 * whether, according to prevailing policy and this Variable's name, its
	 * binding (if any) should be returned in a substitution (i.e. unless it's
	 * anonymous or we're in dont-tell-me mode and its a dont-tell-me variable)
	 *
	 * @return whether, according to prevailing policy and this Variable's name,
	 *         its binding (if any) should be returned in a substitution
	 */
	protected boolean tellThem() {
		return !(this.name.equals("_") || org.jpl7.JPL.modeDontTellMe && this.name.charAt(0) == '_');
	}

	/**
	 * Returns a Prolog source text representation of this Variable
	 *
	 * @return a Prolog source text representation of this Variable
	 */
	public String toString() {
		return this.name;
	}

	/**
	 * returns the type of this subclass of Term, i.e. Prolog.VARIABLE
	 *
	 * @return the type of this subclass of Term, i.e. Prolog.VARIABLE
	 */
	public final int type() {
		return Prolog.VARIABLE;
	}

	/**
	 * returns the typeName of this subclass of Term, i.e. "Variable"
	 *
	 * @return the typeName of this subclass of Term, i.e. "Variable"
	 */
	public String typeName() {
		return "Variable";
	}

}
