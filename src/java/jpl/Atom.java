package jpl;

import java.util.Map;

import jpl.fli.Prolog;
import jpl.fli.StringHolder;
import jpl.fli.term_t;

/**
 * Atom is a specialised Compound with zero arguments, representing a Prolog atom with the same name. An Atom is constructed with a String parameter (its name, unquoted), which cannot thereafter be
 * changed.
 * 
 * <pre>
 * Atom a = new Atom(&quot;hello&quot;);
 * </pre>
 * 
 * An Atom can be used (and re-used) as an argument of Compound Terms. Two Atom instances are equal (by equals()) iff they have equal names.
 * 
 * <hr>
 * <i> Copyright (C) 2004 Paul Singleton
 * <p>
 * Copyright (C) 1998 Fred Dushin
 * <p>
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library Public License as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * <p>
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library Public License for more details.
 * <p>
 * </i>
 * <hr>
 * 
 * @see jpl.Term
 * @see jpl.Compound
 */
public class Atom extends Compound {

	protected final String blobType; // should this be private?

	/**
	 * @param name
	 *            the Atom's name (unquoted)
	 */
	public Atom(String name) {
		super(name);
		this.blobType = "text";
	}

	public Atom(String name, String blobType) {
		super(name);
		if (blobType == null) {
			throw new JPLException("cannot construct with null blobType");
		} else {
			this.blobType = blobType;
		}
	}

	public final int type() {
		return Prolog.ATOM;
	}

	public final String blobType() {
		return this.blobType;
	}

	/**
	 * whether this Term denotes (syntax-specifically) an empty list
	 */
	public boolean isListNil() {
		return this.equals(JPL.LIST_NIL);
	}

	/**
	 * returns the name of the type of this term, as "Atom"
	 * 
	 * @return the name of the type of this term, as "Atom"
	 */
	public String typeName() { // overrides same in jpl.Term
		return "Atom";
	}

	public Object jrefToObject() {
		throw new JPLException("Atom.jrefToObject: term is not a JRef");
	}

	/**
	 * Returns a debug-friendly String representation of an Atom.
	 * 
	 * @return a debug-friendly String representation of an Atom
	 * @deprecated
	 */
	public String debugString() {
		return "(Atom " + toString() + ")";
	}

	/**
	 * Converts a Prolog term (as a term_t), which is known to be a SWI-Prolog string, to a new jpl.Atom, by creating a new Atom object initialised with the string's value. JPL users should avoid
	 * SWI-Prolog's non-ISO strings, but in some obscure circumstances they are returned unavoidably, so we have to handle them (and this is how).
	 * 
	 * @param vars_to_Vars
	 *            A Map from Prolog variables to JPL Variables.
	 * @param term
	 *            The term_t to convert
	 * @return A new Atom instance
	 */
	protected static Term getString(Map<term_t, Variable> vars_to_Vars, term_t term) {
		StringHolder holder = new StringHolder();
		Prolog.get_string_chars(term, holder); // ignore return val; assume success...
		return new Atom(holder.value);
	}

}
