package org.jpl7;

import org.jpl7.fli.Prolog;
import org.jpl7.fli.term_t;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
//import java.util.stream.Collectors;	// include when upgrading to Java 1.8

/**
 * Dict is a specialised Term representing a Prolog dictionary of the form Tag{Key1:Value1, Key2:Value2, ...}.
 * IN SWIPL refer to https://www.swi-prolog.org/pldoc/man?section=bidicts
 *
 * <pre>
 * 	Dict dict1 = new Dict(new Atom("location"), map);
 * 	Dict t3 = (Dict) Term.textToTerm("location{t:12, z:312, y:23}");
 * </pre>
 *
 * <hr>
 * Copyright (C) 2020 Sebastian Sardina
 * <p>
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
public class Dict extends Term {

	/**
	 * The tag of the dictionary - an Atom or a Variable
	 */
	protected Term tag; // was final, but non-recursive Term.get needs to set it later

	/**
	 * 	The mapping of the dictionary
	 */
	protected final Map<Atom, Term> map;

	/**
	 * Creates a dictionary structure
	 *
	 * @param tag		the tag of the dict as an Atom or Variable
	 * @param map		a mapping from Atoms to terms
	 *
	 */
	public Dict(Term tag, Map<Atom, Term> map) {
		this.tag = tag;
		this.map = map;
	}


	public Dict(String dict) {
		// We call Prolog to convert a dict as a String into a list of Terms.
		// Too complex or non-robust (see below) in Java!
		Term t = new Query(String.format("term_to_atom(_A, '%s'), _A =.. [_| X]", dict)).oneSolution().get("X");
//		Term t = Term.textToTerm(dict);		// not working, does not yield a list as needed
		Term[] lt = Term.listToTermArray(t);

		this.tag = (Atom) lt[0];
		this.map = new HashMap<Atom, Term>();
		for(int i = 1; i < lt.length-1; i = i + 2) {
			this.map.put((Atom) lt[i+1], lt[i]);
		}

		// NOT ROBUST METHOD BECAUSE VALUES CAN BE TERMS WITH COMMAS!
//		String mapAsText = "";
//		Pattern p = Pattern.compile("(.+)\\{(.*:.*)\\}");
//		Matcher m = p.matcher(dict);
//		if (m.find()) {
//			this.name = m.group(1);
//			mapAsText = m.group(2);
//
//			try {
//				this.map = Arrays.stream(mapAsText.split(", "))
//						.map(entry -> entry.split(":"))
//						.collect(Collectors.toMap(entry -> new Atom(entry[0]), entry -> Term.textToTerm(entry[1])));
//			} catch (Exception e) {
//				throw new  JPLException(String.format("Not a legal dict format (problem with dictionary): %s", dict));
//			}
//		}	else {
//			throw new  JPLException(String.format("Not a legal dict format: %s", dict));
//		}

	}


	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || !(o instanceof Dict)) return false;
		Dict dict1 = (Dict) o;
		return tag.equals(dict1.tag) &&
				Objects.equals(map, dict1.map);
	}

	@Override
	public int hashCode() {
		return Objects.hash(tag, map);
	}

	public final Term getTag() {
		return tag;
	};
	public final Map<Atom, Term> getMap() {
		return map;
	};

	/**
	 * Whether this Dictionary's functor has 'name' and 'arity' (c.f. traditional functor/3)
	 *
	 * ?- A = point{x:1, y:2}, A =.. [X|Y].
	 * A = point{x:1, y:2},
	 * X = C'dict',
	 * Y = [point, 1, x, 2, y].
	 *
	 * @param  val the name of the function (irrelevant here)
	 * @param arity the arity of the function (size of dict + 1 as tag is added to list)
	 * @return whether this Rational's functor has (long) 'name' and 'arity'
	 */
	public final boolean hasFunctor(Term val, int arity) {
		return this.tag.equals("C'dict'") &&  arity == this.map.size() + 1;
	}

	/**
	 * a Prolog source text representation of this dictionary's value
	 *
	 * @return a Prolog source text representation of this dictionary's value
	 */
	@Override
	public String toString() {
		String mapping = "";

		for (Iterator<Atom> iterator = map.keySet().iterator(); iterator.hasNext();) {
			Atom key= iterator.next();
			Term value = map.get(key);
			mapping = mapping + String.format("%s:%s", key.toString(), value.toString()) ;
			if (iterator.hasNext()) mapping = mapping + ", ";
		}
		return String.format("%s{%s}", tag, mapping);
	}

	//TODO: exchange for this when upgrading to Java 1.8
//	public String toString() {
//		// https://www.baeldung.com/java-map-to-string-conversion
//		String mapAsString = map.keySet().stream()
//				.map(key -> key + ":" + map.get(key))
//				.collect(Collectors.joining(", ", "{", "}"));
//		return String.format("%s%s", tag.toString(), mapAsString);
//	}




	/**
	 * the type of this term, as "Prolog.DICT"
	 *
	 * @return the type of this term, as "Prolog.DICT"
	 */
	public final int type() {
		return Prolog.DICT;
	}

	/**
	 * the name of the type of this term, as "Rational"
	 *
	 * @return the name of the type of this term, as "Rational"
	 */
	public String typeName() {
		return "Dict";
	}

}
