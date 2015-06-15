package org.jpl7.fli;

/**
 * A PointerHolder is a trivial extension of a LongHolder. This is sort of a no-no in Java, as the long value stored herein is sometimes a machine address. (Don't tell Sun.)
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
// Implementation notes:
// There could be issues in the future with signedness, since Java
// does not have an unsigned type; make sure not to do any arithmetic
// with the stored value.
// ----------------------------------------------------------------------/
public class PointerHolder extends LongHolder {
}
