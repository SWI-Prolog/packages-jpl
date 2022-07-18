package org.jpl7;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.jpl7.fli.DoubleHolder;
import org.jpl7.fli.Int64Holder;
import org.jpl7.fli.IntHolder;
import org.jpl7.fli.ObjectHolder;
import org.jpl7.fli.Prolog;
import org.jpl7.fli.StringHolder;
import org.jpl7.fli.TermHolder;
import org.jpl7.fli.term_t;

/**
 * Term is the abstract base class for Compound, Atom, Variable, Integer and Float, which comprise a Java-oriented
 * concrete syntax for Prolog. You cannot create instances of Term directly; rather, you should create instances of
 * Term's concrete subclasses. Alternatively, use textToTerm() to construct a Term from its conventional Prolog source
 * text representation.
 *
 * <hr>
 * Copyright (C) 2020,2004 Paul Singleton
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
public abstract class Term {
	
	public static class PutTask {
		public int n; // 0 -> put t into term; 1+ -> put t.arg(n) into term[n-1]
		public Term t; // the term to be put, if n == 0, else whose n-th arg is to be put
		public term_t term; // the term ref into which the term (or an arg) is to  be put
		public term_t termP; // if not null and n == t.arity() then cons_functor_v from t.name()/t.arity() and term0 into term
		public PutTask prev; // the next task down the stack, if any, else null

		PutTask(int n, Term t, term_t term) {
			this.n = n;
			this.t = t;
			this.term = term;
			this.termP = null;
			this.prev = null;
		}

		PutTask(int n, Term t, term_t term, term_t termP, PutTask prev) {
			this.n = n;
			this.t = t;
			this.term = term;
			this.termP = termP;
			this.prev = prev;
		}
	}
	
	public static class GetTask {
		public int n; // <0 -> get (-n)th dict entry into hTerm.value's map; 0 -> get term into hTerm.value; >0 -> get nth arg of term into hTerm.value.args[n-1]
		public TermHolder hTerm; // holds a new Compound or Dict whose args or entries are being got, or (if n == 0) will hold a got Term 
		public term_t term; // FLI ref to a dict, compound or (if n == 0) anything else
		public int arity; // the arity of a dict, if n < 0, else undefined
		public Term value; // the value of a dict entry whose key is to be got, else undefined
		public GetTask prev; // next task down on stack, if any, else null
		
		GetTask(int n, TermHolder hTerm, term_t term) {
			this.n = n;
			this.hTerm = hTerm;
			this.term = term;
			this.arity = 0;
			this.value = null;
			this.prev = null;
		}

		GetTask(int n, TermHolder hTerm, term_t term, int arity, GetTask prev) {
			this.n = n;
			this.hTerm = hTerm;
			this.term = term;
			this.arity = arity;
			this.value = null;
			this.prev = prev;
		}
	}

	public static class GetSubstsTask {
		public int n; // update varnames_to_Terms from args[n]
		public Term[] args;
		public GetSubstsTask prev;
		
		GetSubstsTask(int n, Term[] args, GetSubstsTask prev) {
			this.n = n;
			this.args = args;
			this.prev = prev;
		}
	}
	
	/**
	 * This default constructor enables subclasses to define their own default constructors
	 */
	protected Term() {
	}

	/**
	 * returns the i-th (1+) argument of a Term;
	 *
	 * defined only for Compound
	 *
	 * @param i the index of argument to return
	 * @return the i-th argument of a (Compound) Term
	 * @throws JPLException if Term is not a Compound
	 */
	public Term arg(int i) { // overridden in Compound
		throw new JPLException("arg(int) is undefined for " + this.typeName() + " instances");
	}

	/**
	 * The arguments of this Term.
	 * <p>
	 * Note that a SWI Prolog 7.x compound term can have zero arguments.
	 * <p>
	 * This method returns an empty Term[] from an Atom, Float, Integer or JRef,
	 * approximating the behaviour of SWI Prolog's =../2
	 *
	 * @return the arguments of a Compound
	 * @throws JPLException
	 *             if the Term is a Variable
	 */
	public Term[] args() { // overridden in Compound
		throw new JPLException("args() is undefined for " + this.typeName());
	}

	/**
	 * the arity of a Compound, Atom, Integer or Float
	 *
	 * @return the arity of a Compound, Atom, Integer or Float
	 * @throws JPLException
	 *             if the Term is a Variable
	 */
	public int arity() { // overridden in Compound, Variable
		return 0;
	}

	/**
	 * @return the type ("text", "string", "reserved_symbol", "jref" etc.) of an Atom
	 * @throws JPLException
	 *             if the Term is not an Atom
	 */
	public String atomType() { // overridden in Atom
		throw new JPLException("atomType() is undefined for " + this.typeName());
	}

	/**
	 * The name of an Atom, Compound or Variable.
	 *
	 * @return the name of an Atom, Compound or Variable.
	 * @throws JPLException
	 *             if this Term is not an Atom, Compound or Variable.
	 */
	public String name() { // overridden in Atom, Compound, Variable
		throw new JPLException("name() is undefined for " + this.typeName());
	}

	public void setName(String name) {
		throw new JPLException("name() is undefined for " + this.typeName());
	}

	/**
	 * Converts a Prolog source text (as a String) to a corresponding JPL Term
	 * (in which each Variable has the appropriate name from the source text).
	 *
	 * @param text A Prolog source text denoting a term
	 * @return Term a JPL Term equivalent to the given source text
	 * @throws PrologException containing error(syntax_error(_),_) if text is invalid as a term.
	 */
	public static Term textToTerm(String text) {
		/*
		 *
		 *  atom_to_term/3 will build a term from a string, and will extract all variables in a list
		 *  https://www.swi-prolog.org/pldoc/doc_for?object=atom_to_term/3
		 *
		 * ?- atom_to_term('mother(maria, S, P)', T, NVdict).
		 * 				T = mother(maria, _4518, _4520),
		 * 				NVdict = ['S'=_4518, 'P'=_4520].
		 *
		 * 	But we want to return the Term mother(maria, S, P) where S and P are Variable Terms referring
		 * 	to _4518 and _4520
		 *
		 * 	To do so, we rename the name of term Variable  _4518 to 'S' and of _4520 to 'P"
		*/

		// it might be better to use PL_chars_to_term()
		//	like; Prolog.put_atom_chars(term, name)
		Query q = new Query(new Compound("atom_to_term",	// build atom_to_term(text, T, NVdict)
				new Term[] {
						new Atom(text),
						new Variable("T"),
						new Variable("NVdict") }));

		q.open();	// redundant because q.hasMoreSolutions() will open it if needed, but leave it for completness
		if (q.hasMoreSolutions()) {
			Map<String, Term> s = q.nextSolution();	// issue query atom_to_term(text, T, NVdict)

			/*	New way of building a Term from a String (April 2020)

					Query atom_to_term/3 will bound variable T above as a proper Term but with anonymous
						variables with names _1 and _2, instead of X and Y.
						For example, T = mother(maria, _4518, _4520)

					However, the mapping between those _ variables and the actual textual names X and Y have been
						"stored" in variable NVdict as a list ['X' = _1, 'X' = _2, ...]

					So, we navigate NVdict and change the name of each anonymous Variable _ to be its textual name!

					Since the Term bound to T will use exactly the Variables _1, _2, etc, by changing their names
						the term T will now use Variables X, Y, etc as wanted!
			*/
			Term[] VarsMapList = Term.listToTermArray(s.get("NVdict"));	// this is a Compound list of mappings
			for (Term map : VarsMapList) { // map represents 'X' = _1
				String VarName = map.arg(1).name();		// extract the symbolic name of the variable
				Variable Var = (Variable) map.arg(2);	// extract the Variable object
				Var.setName(VarName);					// set the name of the Variable object to the symbolic name
			}

			// Old way of doing it (before April 2020) - extremely involved!
			//Map<String, Term> s = q.getSolutionWithVarNames();


			// We MUST close the query explicitly or otherwise it will remain at the top of stack because we
			// just did q.nextSolution() and there may be more
			q.close();

			return s.get("T");
		} else {
			q.close();	// redundant as q.hasMoreSolutions() would have closed it, but ....
			return null;
		}
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
	 * returns the value (as an int) of an Integer or Float
	 *
	 * @return the value (as an int) of an Integer or Float
	 * @throws JPLException
	 *             if this Term is a Compound, Atom or Variable
	 */
	public int intValue() {
		throw new JPLException("intValue() is undefined for " + this.typeName());
	}

	/**
	 * The (long) value of a Float or Integer.
	 *
	 * @return the (long) value of a Float or Integer.
	 * @throws JPLException
	 *             if this Term is not a Float or Integer.
	 */
	public long longValue() { // overridden in Integer, GFloat
		throw new JPLException("longValue() is undefined for " + this.typeName());
	}

	/**
	 * the value (as a java.math.BigInteger) of an Integer, whether or not it is big
	 *
	 * @return the value (as a java.math.BigInteger) of an Integer, whether or not it is big
	 * @throws JPLException
	 *             if the Term is not an Integer
	 */
	public BigInteger bigValue() {
		throw new JPLException("bigValue() is undefined for " + this.typeName());
	}

	/**
	 * the value (as a double) of an Integer or Float
	 *
	 * @return the value (as a double) of an Integer or Float
	 * @throws JPLException
	 *             if the Term is neither an Integer nor a Float
	 */
	public double doubleValue() {
		throw new JPLException("doubleValue() is undefined for " + this.typeName());
	}

	/**
	 * the value (as a float) of an Integer or Float
	 *
	 * @return the value (as a float) of an Integer or Float
	 * @throws JPLException
	 *             if the Term is neither an Integer nor a Float
	 */
	public float floatValue() {
		throw new JPLException("floatValue() is undefined for " + this.typeName());
	}


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
		return getLoop(new GetTask(0, new TermHolder(), term), vars_to_Vars);
	}

	protected static Term getTerm(term_t term) {
		return getTerm(new HashMap<term_t, Variable>(), term);
	}

	/**
	 * Whether this Term's functor has 'name' and 'arity' (c.f. behaviour of SWI Prolog's functor/3)
	 *
	 * @param name
	 *            a possible name for the functor of a term
	 * @param arity
	 *            an arity 0+
	 * @return whether this Term's functor has 'name' and 'arity'
	 * @throws JPLException
	 *             if this Term is a Variable
	 */
	public boolean hasFunctor(String name, int arity) { // overridden in
		// Atom, Compound
		return false;
	}

	/**
	 * Tests whether this Term's functor has 'name' and 'arity'
	 * <p>
	 * For Float and Integer, mimics behaviour of SWI Prolog's functor/3
	 *
	 * @param name
	 *            a possible name for the functor of a term
	 * @param arity
	 *            an arity 0+
	 * @return whether this Term's functor has 'name' and 'arity'
	 * @throws JPLException
	 *             if this Term is a Variable
	 */
	public boolean hasFunctor(long name, int arity) {
		return false;
	}

	/**
	 * Tests whether this Term's functor has 'name' and 'arity'
	 * <p>
	 * For Float and Integer, mimics behaviour of SWI Prolog's functor/3
	 *
	 * @param name
	 *            a possible name for the functor of a term
	 * @param arity
	 *            an arity 0+
	 * @return whether this Term's functor has 'name' and 'arity'
	 * @throws JPLException
	 *             if this Term is a Variable
	 */
	public boolean hasFunctor(BigInteger name, int arity) {
		return false;
	}

	/**
	 * Tests whether this Term's functor has 'name' and 'arity'
	 * <p>
	 * For Float and Integer, mimics behaviour of SWI Prolog's functor/3
	 *
	 * @param name
	 *            a possible name for the functor of a term
	 * @param arity
	 *            an arity 0+
	 * @return whether this Term's functor has 'name' and 'arity'
	 * @throws JPLException
	 *             if this Term is a Variable
	 */
	public boolean hasFunctor(double name, int arity) {
		return false;
	}



	/**
	 * whether this Term is an Atom (of any type)
	 *
	 * @return whether this Term is an Atom (of any type)
	 */
	public final boolean isAtom() {
		return this instanceof Atom;
	}

	/**
	 * Tests whether this Term is an Atom with name and type.
	 *
	 * @param name
	 *            any String
	 * @param type
	 *            an Atom (blob) type, e.g. "text", "reserved_symbol", "string", "jref"
	 * @return whether this Term is an Atom with name and type
	 */
	protected boolean isAtomOfNameType(String name, String type) {
		return false;
	}

	/**
	 * Tests whether this Integer's value is too big to represent as a long.
	 * <p>
	 * Use this in contexts where the Term is known to be an Integer.
	 *
	 * @return whether this Integer's value is too big to represent as a long
	 * @throws JPLException
	 *             if Term is not an org.jpl7.Integer
	 * @see org.jpl7.Term#isBigInteger()
	 */
	public boolean isBig() {
		throw new JPLException("isBig() is undefined for " + this.typeName());
	}

	/**
	 * Tests whether this Term is an Integer whose value is too big to represent as a long
	 *
	 * @return whether this Term is an Integer whose value is too big to represent as a long
	 * @see org.jpl7.Term#isBig()
	 */
	public boolean isBigInteger() {
		return this instanceof Integer && ((Integer) this).isBig();
	}

	/**
	 * Tests whether this Term is a Compound.
	 *
	 * @return whether this Term is a Compound.
	 */
	public final boolean isCompound() {
		return this instanceof Compound;
	}

	/**
	 * Tests whether this Term is an org.jpl7.Float.
	 *
	 * @return whether this Term is an org.jpl7.Float.
	 */
	public final boolean isFloat() {
		return this instanceof Float;
	}

	/**
	 * Tests whether this Term is an org.jpl7.Integer.
	 *
	 * @return whether this Term is an org.jpl7.Integer.
	 */
	public final boolean isInteger() {
		return this instanceof Integer;
	}

	/**
	 * Tests whether this Term is a 'jfalse' structure, i.e. @(false).
	 *
	 * @return whether this Term is a 'jfalse' structure, i.e. @(false).
	 */
	public boolean isJFalse() { // overridden in Compound, where it might sometimes be true
		return false;
	}

	/**
	 * Tests whether this Term is a 'jnull' structure, i.e. @(null).
	 *
	 * @return whether this Term is a 'jnull' structure, i.e. @(null).
	 */
	public boolean isJNull() { // overridden in Compound, where it might sometimes be true
		return false;
	}

	/**
	 * Tests whether this Term is a (non-null, non-String) JPL reference to a Java object, e.g. &lt;jref&gt;(0x1234560)
	 *
	 * @return whether this Term is a (non-null, non-String) reference to a Java object, e.g. &lt;jref&gt;(0x1234560)
	 * @see org.jpl7.JRef#object()
	 */
	public final boolean isJRef() {
		return this instanceof JRef;
	}

	/**
	 * Tests whether this Term is a 'jtrue' structure, i.e. @(true).
	 *
	 * @return whether this Term is a 'jtrue' structure, i.e. @(true).
	 */
	public boolean isJTrue() { // overridden in Compound, where it might sometimes be true
		return false;
	}

	/**
	 * Tests whether this Term is a 'jvoid' structure, i.e. @(void).
	 *
	 * @return whether this Term is a 'jvoid' structure, i.e. @(void).
	 */
	public boolean isJVoid() { // overridden in Compound, where it might sometimes be true
		return false;
	}



	/**
	 * Tests whether this Term is a Variable.
	 *
	 * @return whether this Term is a Variable.
	 */
	public final boolean isVariable() {
		return this instanceof Variable;
	}



	/**
	 * The Object which this org.jpl7.JRef refers to, iff this Term is a JRef or just JPL.JNULL.
	 *
	 * @return the Object which this Term refers to, iff this Term is a JRef.
	 * @throws JPLException if this Term is not a JRef or not NULL term
	 */
	public Object object() { // overridden in JRef
		if (this == JPL.JNULL)	// address comparison: exactly JPL.JNULL object
			return null;
		else
			throw new JPLException("term is neither a JRef nor a Compound representing @(null)");
	}

	/**
	 * Returns the JREF term for an object
	 *
	 * @param object object of interest
	 * @return a new JRef which references object, or @(null) if object == null.
	 * @throws JPLException
	 *             if object is a String.
	 * @deprecated Use {@link JPL#newJRef}
	 */
	@Deprecated
	public static final Term objectToJRef(Object object) {
		return JPL.newJRef(object);
	}

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
		putTerm(this, new HashMap<String, term_t>(), term);
	}

	protected static void putTerm(Term t, Map<String, term_t> varnames_to_vars, term_t term) {
		putLoop(new PutTask(0, t, term), varnames_to_vars);
	}

	protected static void putLoop(PutTask task0, Map<String, term_t> varnames_to_vars) {
		PutTask top = task0; // initially, our stack is just the given task
		while (top != null) { // while there's a (top-most) task to do
			if (top.n > top.t.arity()) { // we have populated a vector of term refs for a Compound, and (only) now can we cons it 
				if (top.termP != null) { // it's time to stash the populated Compound into this "parent" term ref
					Prolog.cons_functor_v(top.termP, Prolog.new_functor(Prolog.new_atom(top.t.name()), top.t.arity()), top.term);
				} else {
					// we were presumably just stashing args for Query.open()
				}
				top = top.prev; // pop this completed task and iterate
			} else {
				Term t; // the Term which the switch will deal with in this iteration
				term_t term; // the term ref into which the switch will put t in this iteration
				if (top.n == 0) { // put t into term
					t = top.t; // the Term to be put
					term = top.term; // the term ref to put it in
					top = top.prev; // pop this imminently completed task now lest we enstack a Compound task
				} else { // put n-th arg of t into corresponding element of (0-based) term ref array
					t = top.t.arg(top.n); // Term.arg() is 1-based
					if (top.n == 1) {
						term = top.term; // put t into the given term ref
					} else { // top.n > 1
						term = new term_t(); // there isn't a term_t(long value) constructor :-/
						term.value = top.term.value + (top.n - 1); // n is 1-based but term ref array is 0-based
					}
					top.n++; // increment n for a subsequent iteration (safe: not referred to in switch) 
				}
				switch (t.type()) {
				case Prolog.ATOM:
					if (t.equals(JPL.LIST_NIL)) {
						Prolog.put_nil(term);
					} else {
						Prolog.put_atom_chars(term, t.name());
					}
					break;
				case Prolog.COMPOUND:
					top = new PutTask(1, t, Prolog.new_term_refs(t.arity()), term, top); // push a new task, to put t's args starting with the first
					break;
				case Prolog.DICT:
					Prolog.put_rational(term, t.toString()); // works because it is purely syntactic
					break;
				case Prolog.FLOAT:
					Prolog.put_float(term, t.floatValue());
					break;
				case Prolog.INTEGER:
					if (t.isBig()) {
						Prolog.put_integer_big(term, t.bigValue().toString());
					} else {
						Prolog.put_integer(term, t.longValue());
					}
					break;
				case Prolog.JREF:
					Prolog.put_jref(term, t.object());
					break;
				case Prolog.RATIONAL:
					Prolog.put_rational(term, t.toString());
					break;
				case Prolog.VARIABLE:
					// To put a Variable, we must check whether a (non-anonymous) variable with
					// the same name has already been put from this Term. If one has, then the
					// corresponding Prolog variable has been stashed in the varnames_to_vars
					// Map, keyed by the Variable's name, so we can look it up and reuse it (this
					// way, the sharing of variables in the Prolog term reflects the sharing of
					// Variable names in the Term). If a non-anonymous Variable's name has not
					// already been seen in the Term, then we put a new Prolog variable and add
					// it into the Map (keyed by this Variable's name).
					term_t var;
					if (t.name().equals("_") || (var = varnames_to_vars.get(t.name())) == null) {
						((Variable) t).term_ = term;
						((Variable) t).index = varnames_to_vars.size(); // i.e. first var in is #0 etc.
						Prolog.put_variable(term);
						if (!t.name().equals("_")) {
							varnames_to_vars.put(t.name(), term);
						}
					} else {
						((Variable) t).term_ = var;
						Prolog.put_term(term, var);
					}
					break;
				default:
					throw(new JPLException("bad term type in Term.putLoop/2"));
				}
			}
		}
	}
	
	/**
	 * This internal method is public because it needs to be callable via JNI, but it is not part of JPL's public API
	 * and should not be used by applications.
	 *
	 * @param params a list of terms to fill the ? placeholders in the Term object
	 * @return a new term representing the original term with all its placeholders ? replaced by the corresponding
	 * 			successive parameter
	 * @throws JPLException if there are more actual than formal parameters.
	 */
	public Term putParams(Term[] params) { // necessarily (?) public
		IntHolder next = new IntHolder();
		next.value = 0;
		Term t2 = this.putParams1(next, params);
		if (next.value != params.length) {
			throw new JPLException("more actual params than formal");
		}
		return t2;
	}

	// TODO: is this used at all? Can we remove it?
	protected Term putParams(Term plist) { // was public
		Term[] ps = plist.listToTermArray();
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

	// called by jni_jobject_to_term_byval/2 in jpl.c, hence necessarily public
	public static void putTerm(Object obj, term_t termref) {
		if (obj instanceof Term) {
			putTerm((Term) obj, new HashMap<String, term_t>(), termref);
		} else {
			throw new JPLException("not a Term");
		}
	}

	/**
	 * Called only from Query.open(), this static method puts the arguments of a goal (Term) into *consecutive* term_t objects. Note that the first
	 * term_t object returned is a term_t class (structure); the succeeding term_t objects are consecutive references
	 * obtained by incrementing the *value* field of the term_t.
	 *
	 * @param varnames_to_vars
	 *            Map from variable names to JPL Variables.
	 * @param t
	 *            An array of org.jpl7.Term references.
	 * @return a term_t wrapping the first of t.arity() term refs into which t's args have now been put
	 */
	protected static term_t putArgs(Term t, Map<String, term_t> varnames_to_vars) {
		// First create a sequence of term_ts.
		// The 0th term_t will be wrapped in an org.jpl7.fli.term_t.
		// Successive Prolog term_t references will reside in the Prolog engine, and can be obtained by term0.value+i.
		term_t term0 = Prolog.new_term_refs(t.arity());
		putLoop(new PutTask(1, t, term0), varnames_to_vars);
		return term0;
	}

	/**
	 * The (non-null, non-String) object which this org.jpl7.JRef references.
	 *
	 * @return the (non-null, non-String) object which this org.jpl7.JRef references.
	 * @throws JPLException
	 *             if this Term is not a JRef
	 * @deprecated Use {@link JRef#object()}
	 */
	@Deprecated
	public Object ref() { // overridden in JRef
		throw new JPLException("this Term is not a JRef");
	}


	/**
	 * @return the Object which this JRef references
	 * @deprecated Use {@link Term#object()}
	 */
	@Deprecated
	public Object jrefToObject() { // overridden in Compound and JRef
		return this.object();
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
	 * This method computes a substitution from a Term (the current object).
	 * The bindings Map varnames_to_Terms maps names of Variables to Terms.
	 * Thus, a substitution is as it is in mathematical logic, a sequence of the form \sigma = {t_0/x_0, ..., t_n/x_n}.
	 * Once the substitution is computed, the substitution should satisfy
	 *
	 * \sigma T = t
	 *
	 * where T is the Term from which the substitution is computed, and t is the term_t which results from the Prolog
	 * query.
	 * <p>
	 *
	 * A second Map, vars_to_Vars, is required: this table holds the Variables that occur (thus far) in the unified term.
	 * 	The Variable instances in this table are guaranteed to be unique and are keyed on Strings which are Prolog internal
	 * representations of the variables.
	 *
	 * @param varnames_to_Terms
	 *            table holding Term substitutions, keyed on names of Variables.
	 * @param vars_to_Vars
	 *            A Map holding the Variables that occur thus far in the term; keyed by internal (Prolog) string rep.
	 */
	protected void getSubst(Map<String, Term> varnames_to_Terms, Map<term_t, Variable> vars_to_Vars) {
		// overridden in Compound, Variable
	}

	/**
	 * Just calls computeSubstitution for each Term in the array.
	 *
	 * @param varnames_to_Terms
	 *            a Map from variable names to Terms (what each variable string is to be replaced by)
	 * @param vars_to_Vars
	 *            a Map from Prolog variables (which may be bounded in the engine) to JPL Variables (which are Java objects)
	 * @param args
	 *            an array of Terms to which the substitution is to be applied
	 */
	protected static void getSubsts(Map<String, Term> varnames_to_Terms, Map<term_t, Variable> vars_to_Vars,
									Term[] args) {
		getSubstsLoop(varnames_to_Terms, vars_to_Vars, args);
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

	/**
	 * returns the type of this term, as one of org.jpl7.fli.Prolog.COMPOUND, .ATOM, .VARIABLE, .INTEGER, .FLOAT etc
	 *
	 * @return the type of this term, as one of org.jpl7.fli.Prolog.COMPOUND, .ATOM, .VARIABLE, .INTEGER, .FLOAT etc
	 */
	public abstract int type();

	/**
	 * returns the name of the type of this term, as one of "Compound", "Atom", "Variable", "Integer", "Float" or "JRef"
	 *
	 * @return the name of the type of this term, as one of "Compound", "Atom", "Variable", "Integer", "Float" or "JRef"
	 */
	public abstract String typeName();





	// ==================================================================/
	// LIST METHODS
	//
	// All tools to deal specifically with lists that could be either
	// empty list JPL.LIST_NIL or Compound terms with functor [|].
	// ==================================================================/

	/**
	 * Tests whether this Term denotes an empty list within the current syntax ("traditional" or "modern").
	 *
	 * @return whether this Term denotes an empty list within the current syntax ("traditional" or "modern").
	 * @see org.jpl7.JPL#getSyntax()
	 */
	public boolean isListNil() { // overridden in Atom
		return false;
	}

	/**
	 * Tests whether this Term is a list pair within the current
	 * syntax ("traditional" or "modern").
	 *
	 * Note that a list pair may not be a list itself and hence isList() will return false
	 * as the second argument many not be a list
	 *
	 * @return whether this Term is a list pair within the current syntax ("traditional" or "modern").
	 */
	public boolean isListPair() { // overridden in Compound
		return false;
	}

	/**
	 * whether the Term represents a proper list
	 *
	 * @return whether the Term represents a proper list
	 */
	public final boolean isList() {
		return isList(this);
	}

	/**
	 * Whether the Term represents a proper list
	 *
	 * @param term the term to check if it is a list
	 * @return whether the Term represents a proper list
	 */
	public static final boolean isList(Term term) {
		return listLength(term) >= 0;
	}



	/**
	 * Returns the length of a list
	 *
	 * @param term any Term
	 * @return the length of the proper list which the Term represents, else -1
	 */
	public static int listLength(Term term) {
		int length = 0;
		Term head = term;
		while (head.isListPair()) {
			length++;
			head = head.arg(2);
		}
		return (head.isListNil() ? length : -1);
	}

	/**
	 * @return the length of the proper list which the Term represents, else -1
	 */
	public int listLength() {
		return Term.listLength(this);
	}

	/**
	 * Converts an array of String to a corresponding JPL list of atoms
	 *
	 * @param a An array of String objects
	 * @return Term a JPL list of atoms corresponding to the given String array
	 */
	public static Term stringArrayToList(String[] a) {
		Term list = JPL.LIST_NIL;
		for (int i = a.length - 1; i >= 0; i--) {
			list = new Compound(JPL.LIST_PAIR, new Term[] { new Atom(a[i]), list });
		}
		return list;
	}

	/**
	 * Converts an array of int to a corresponding JPL list
	 *
	 * @param a An array of int values
	 * @return Term a JPL list corresponding to the given int array
	 */
	public static Term intArrayToList(int[] a) {
		Term list = JPL.LIST_NIL; // was new Atom("[]");
		for (int i = a.length - 1; i >= 0; i--) {
			list = new Compound(JPL.LIST_PAIR, new Term[] { new org.jpl7.Integer(a[i]), list });
		}
		return list;
	}

	/**
	 * Converts an array of arrays of int to a corresponding JPL list of lists
	 *
	 * @param a An array of arrays of int values
	 * @return Term a JPL list of lists corresponding to the given int array of arrays
	 */
	public static Term intArrayArrayToList(int[][] a) {
		Term list = JPL.LIST_NIL; // was new Atom("[]");
		for (int i = a.length - 1; i >= 0; i--) {
			list = new Compound(JPL.LIST_PAIR, new Term[] { intArrayToList(a[i]), list });
		}
		return list;
	}


	/**
	 * Converts an array of Terms to a JPL representation of a Prolog list of terms whose members correspond to the
	 * respective array elements.
	 *
	 * @param terms
	 *            An array of Term
	 * @return Term a list of the array elements
	 */
	public static Term termArrayToList(Term[] terms) {
		Term list = JPL.LIST_NIL; // was new Atom("[]")
		for (int i = terms.length - 1; i >= 0; --i) {
			list = new Compound(JPL.LIST_PAIR, new Term[] { terms[i], list });
		}
		return list;
	}


	/**
	 * Converts a term representing a list of atoms into an array of Strings, each element
	 * in the array being a String for the corresponding atom
	 * 	e.g., [a, b, 1
	 *
	 *
	 * @param t a term representing a list of atoms
	 * @return an array of Strings, each element, null if t is not a list of atoms
	 */
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



	/**
	 * Converts a proper list to an array of terms, else throws an exception
	 *
	 * @param t the Term representing a list
	 * @throws JPLException if the term passed is not itself a Prolog list term
	 * @return an array of terms whose successive elements are the corresponding members of the list (if it is a list)
	 */
	public static Term[] listToTermArray(Term t) {
		int len = Term.listLength(t); // exception if not a well formed list
		if (len < 0) {
			throw new JPLException("term is not a proper list");
		}
		Term[] ts = new Term[len];
		for (int i = 0; i < len; i++) { // no need to check functor (it's a list)
			ts[i] = t.arg(1);
			t = t.arg(2);
		}
		return ts;
	}

	/**
	 * Converts a proper list to an array of terms, else throws an exception
	 *
	 * @throws JPLException if the term passed is not itself a Prolog list term
	 * @return an array of terms whose successive elements are the corresponding members of the list (if it is a list)
	 */
	public final Term[] listToTermArray() {
		return Term.listToTermArray(this);
	}

	/**
	 * @return an array of terms whose successive elements are the corresponding members of the list (if it is a list)
	 *
	 * @deprecated Use {@link Term#listToTermArray()}
	 */
	@Deprecated
	public final Term[] toTermArray() {
		return listToTermArray();
	}

	protected static Term getLoop(GetTask task0, Map<term_t, Variable> vars_to_Vars) {
		GetTask top = task0; // initially, our stack is just the given task
		StringHolder hString = new StringHolder(); // used & reused within switch
		StringHolder hName = new StringHolder(); // used & reused within switch
		IntHolder hArity = new IntHolder(); // used & reused within switch
		Int64Holder hInt64 = new Int64Holder(); // used & reused within switch
		DoubleHolder hDouble = new DoubleHolder(); // used & reused within switch
		ObjectHolder hObject = new ObjectHolder(); // used & reused within switch
		while (top != null) { // there is a top-most task to do next
			GetTask task = top; // in case we pop or push before we've finished referring to it
			term_t term; // temp for a term ref for the switch, set appropriately before
			Term t; // temp for a Term from the switch, stashed appropriately after
			// set up term for the switch, according to task, and pop task if complete this cycle
			if (task.n > 0) { // get n-th arg of compound in task.term
				term = Prolog.new_term_ref();
				Prolog.get_arg(task.n, task.term, term); // set up term as ref to the n-th arg of compound
				if (task.n == task.arity) { // this recurrent task gets the last arg of a compound
					top = top.prev; // pop this recurrent args-of-compound task (complete this cycle)
				}
			} else if (top.n < 0) { // get (-n)th arg of dict in task.term
				term = Prolog.new_term_ref();
				Prolog.get_arg(-task.n, task.term, term); // set up term as ref to (-n)-th arg of dict
				if (-task.n == task.arity) { // this recurrent task gets the last arg of a dict
					top = top.prev; // pop this recurrent args-of-dict task (complete this cycle)
				}
			} else { // special, initial case: just get a term
				term = task.term; // get the term in task.term
				top = top.prev; // pop this simple task (complete this cycle)
			}
			switch (Prolog.term_type(term)) { // get term into t (to be stashed appropriately later)
			case Prolog.VARIABLE:
				t = null; // provisionally, until/unless set within this for loop
				for (Iterator<term_t> it = vars_to_Vars.keySet().iterator(); it.hasNext();) {
					term_t varX = it.next(); // a previously seen Prolog variable
					if (Prolog.compare(varX, term) == 0) { // referenced variables are identical
						t = (Term) vars_to_Vars.get(varX); // yield this (reused) Variable
						break; // out of enclosing for loop
					}
				}
				if (t == null) { // the Prolog variable in term has not been seen so far in this get
					Variable Var = new Variable(); // allocate a new (sequentially named, e.g. "_163") Variable
					Var.term_ = term; // TODO understand this :-/
					vars_to_Vars.put(term, Var); // enmap new Variable, keyed by term ref, lest we see it again
					t = Var; // yield this new Variable
				} // else a reused Variable was assigned to t in the for loop above
				break;
			case Prolog.ATOM: // specifically a "text" atom (see also Prolog.BLOB)
				if (Prolog.get_atom_chars(term, hString)) {
					t = new Atom(hString.value, "text");
				} else {
					throw new JPLException("Prolog.get_atom_chars failed");
				}
				break;
			case Prolog.STRING:
				if (Prolog.get_string_chars(term, hString)) {
					t = new Atom(hString.value, "string");
				} else {
					throw new JPLException("Prolog.get_string_chars failed");
				}
				break;
			case Prolog.INTEGER:
				if (Prolog.get_integer(term, hInt64)) { // assume failure means it's too big for a Java long
					t = new org.jpl7.Integer(hInt64.value); // fully qualified to avoid java.lang.Integer
				} else {
					if (Prolog.get_integer_big(term, hString)) {
						t = new org.jpl7.Integer(new java.math.BigInteger(hString.value)); // ditto
					} else {
						throw new JPLException("Prolog.get_integer and Prolog.get_integer_big failed");
					}
				}
				break;
			case Prolog.BLOB:
				if (Prolog.get_jref_object(term, hObject)) {
					t = (hObject.value == null ? JPL.JNULL : new JRef(hObject.value));
				} else {
					throw new JPLException("unsupported blob type passed from Prolog");
				}
				break;
			case Prolog.COMPOUND:
			case Prolog.LIST_PAIR:
				if (Prolog.get_name_arity(term, hName, hArity)) {
					t = new Compound(hName.value, new Term[hArity.value]);
					if (hArity.value > 0) { // since SWIPL7, compounds can have zero arity :-/
						top = new GetTask(1, new TermHolder(t), term, hArity.value, top); // push recurring args-of-compound task
					}
				} else {
					throw new JPLException("Prolog.get_name_arity failed");
				}
				break;
			case Prolog.DICT: // term is dict(tag,value1,key1,value2,key2,..); TODO: keys can also be "small" integers
				if (Prolog.get_name_arity(term, hName, hArity)) {
					t = new Dict(null, new HashMap<Atom, Term>((hArity.value-1)/2)); // tag will be set by args-of-dict task
					top = new GetTask(-1, new TermHolder(t), term, hArity.value, top); // push recurring args-of-dict task
				} else {
					throw new JPLException("Prolog.get_name_arity failed on a dict");
				}
				break;
			case Prolog.FLOAT:
				if (Prolog.get_float(term, hDouble)) {
					t = new org.jpl7.Float(hDouble.value); // fully qualified to avoid java.lang.Float
				} else {
					throw new JPLException("Prolog.get_float failed");
				}
				break;
			case Prolog.LIST_NIL:
				t = JPL.LIST_NIL;
				break;
			case Prolog.RATIONAL:
				if (Prolog.get_rational(term, hString)) {
					t = new Rational(hString.value);
				} else {
					throw new JPLException("Prolog.get_rational failed");
				}
				break;
			default: // should never happen
				throw new JPLException("unknown term type=" + Prolog.term_type(term));
			}
			if (task.n > 0) { // we've just got, into t, the n-th arg of the Compound already (skeletally) in hTerm
				task.hTerm.t.args()[task.n-1] = t; // stash the just-got Term t into the appropriate element of the skeletal Compound's (0-based) Term[] args
				if (task.n < task.arity) { // there are more args to be got
					task.n++; // update this recurrent task to get the next arg
				}
			} else if (task.n < 0) { // we're getting the args (tag, value or key) of a dict
				if (-task.n == 1) { // we just got the tag (either an Atom or a Variable)
					((Dict) task.hTerm.t).tag = t; // set the tag of the already-created, but skeletal, Dict
					if (-task.n < task.arity) { // this dict has at least one entry
						top.n--; // update this recurrent task to get the first value 
					}
				} else if ((-task.n)%2 == 0) { // we got a value (n is even)
					task.n--; // update task to get this value's key
					task.value = t; // save it in the task until next cycle
				} else { // we got a key into t (odd n)
					((Dict) task.hTerm.t).map.put((Atom) t, task.value); // value from previous cycle
					if (-task.n < task.arity) { // not the last entry
						task.n--; // update task to get the next value
					}
				}
			} else { // simple (initial) task; we got a term 
				task.hTerm.t = t;
			}
		}
		return task0.hTerm.t; // when all tasks are complete, overall Term is in here
	}

	protected static void getSubstsLoop(Map<String, Term> varnames_to_Terms, Map<term_t, Variable> vars_to_Vars, Term[] args) {
		GetSubstsTask top = (args.length > 0 ? new GetSubstsTask(0, args, null) : null); // prime the stack
		while (top != null) {
			GetSubstsTask task = top;
			Term t = task.args[task.n];
			if (task.n+1 < task.args.length) { // there are further args to scan
				task.n++; // update this recurrent task for next arg and leave it on the stack
			} else {
				top = top.prev; // pop this now-completed recurrent task
			}
			switch (t.type()) {
			case Prolog.VARIABLE:
				Variable v = (Variable) t;
				if (v.tellThem() && varnames_to_Terms.get(v.name) == null) {
					varnames_to_Terms.put(v.name, Term.getTerm(vars_to_Vars, v.term_));
				}
				break;
			case Prolog.COMPOUND:
			case Prolog.LIST_PAIR:
				if (t.args().length > 0) { // this Compound has at least one arg
					top = new GetSubstsTask(0, t.args(), top);
				} else {
					// zero-arity Compound; no subterms to traverse
				}
				break;
			case Prolog.ATOM:
			case Prolog.BLOB:
			case Prolog.DICT:
			case Prolog.FLOAT:
			case Prolog.INTEGER:
			case Prolog.JREF:
			case Prolog.LIST_NIL:
			case Prolog.RATIONAL:
			case Prolog.STRING:
				break; // nothing to do for these term types
			default: // should never happen
				throw new JPLException("unknown term type=" + t.type());
			}
		}
	}

}