package org.jpl7.fli;

/**
 * An atom_t is a specialised LongHolder which decrements its atom's reference count when garbage-collected (finalized).
 * 
 * <hr>
 * <i> Copyright (C) 1998 Fred Dushin
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
 */
public class atom_t extends LongHolder {
	/**
	 * The String representation of an atom_t is just the atom's name.
	 * 
	 * @return atom's name
	 */
	public String toString() {
		return Prolog.atom_chars(this);
	}

	protected void finalize() throws Throwable {
		super.finalize();
		Prolog.unregister_atom(this);
	}
}
