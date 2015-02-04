package jpl;

import java.util.Map;
import jpl.fli.Prolog;
import jpl.fli.term_t;

/**
 * JRef is a specialised Term with an Object field, representing JPL's Prolog references to Java objects (or to null).
 * 
 * <pre>
 * JRef r = new JRef(non_String_object_or_null);
 * </pre>
 * 
 * A JRef can be used (and re-used) in Compound Terms.
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
 * @author Fred Dushin <fadushin@syr.edu>
 * @version $Revision$
 * @see jpl.Term
 * @see jpl.Compound
 * 
 * @deprecated
 */
public class JRef extends Term {

	/**
	 * the JRef's value (a non-String Object or null)
	 */
	protected final Object ref;

	/**
	 * This constructor creates a JRef, initialized with the supplied non-String object (or null).
	 * 
	 * @param ref
	 *            this JRef's value (a non-String object, or null)
	 */
	public JRef(Object ref) {
		if (ref instanceof String) {
			throw new JPLException("a JRef cannot have a String value (String maps to atom)");
		} else {
			this.ref = ref;
		}
	}

	public Term arg(int ano) {
		return (ano == 1 ? new Atom(jpl.fli.Prolog.object_to_tag(ref)) : null);
	}

	/**
	 * Returns a Prolog source text representation of this JRef
	 * 
	 * @return a Prolog source text representation of this JRef
	 */
	public String toString() {
		return "" + ref + ""; // WRONG
	}

	/**
	 * Two JRefs are equal if their references are identical (?)
	 * 
	 * @param obj
	 *            The Object to compare
	 * @return true if the Object satisfies the above condition
	 */
	public final boolean equals(Object obj) {
		return this == obj || (obj instanceof JRef && ref == ((JRef) obj).ref);
	}

	public final int type() {
		return Prolog.JREF;
	}

	public String typeName() {
		return "JRef";
	}

	/**
	 * The non-String object (or null) which this jpl.JRef represents
	 * 
	 * @return the non-String object (or null) which this jpl.JRef represents
	 */
	public Object ref() {
		return ref;
	}

	/**
	 * Returns a debug-friendly representation of this JRef
	 * 
	 * @return a debug-friendly representation of this JRef
	 * @deprecated
	 */
	public String debugString() {
		return "(JRef " + toString() + ")";
	}

	/**
	 * To convert a JRef to a term, we put its Object field (.value) into the term_t as a JPL ref (i.e. @/1) structure.
	 * 
	 * @param varnames_to_vars
	 *            A Map from variable names to Prolog variables.
	 * @param term
	 *            A (newly created) term_t which is to be set to a Prolog 'ref' (i.e. @/1) structure denoting the .value of this JRef instance
	 */
	protected final void put(Map<String, term_t> varnames_to_vars, term_t term) {
		Prolog.put_jref(term, ref);
	}

	public boolean hasFunctor(String name, int arity) {
		return name != null && name.equals("@") && arity == 1;
	}

	public Object jrefToObject() {
		return ref;
	}

}
