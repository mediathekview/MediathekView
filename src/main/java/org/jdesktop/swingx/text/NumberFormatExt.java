/*
 * $Id$
 *
 * Copyright 2009 Sun Microsystems, Inc., 4150 Network Circle,
 * Santa Clara, California 95054, U.S.A. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */
package org.jdesktop.swingx.text;

import java.text.*;

/**
 * A specialised NumberFormat which handles null values and empty Strings. 
 * This is useful in cell editors and used in StrictNumberFormatter.
 * 
 * @author Noel Grandin
 * @author Jeanette Winzenburg
 */
public class NumberFormatExt extends NumberFormat {
    
    private NumberFormat childFormat;

    public NumberFormatExt() {
        this(null);
    }
    
    public NumberFormatExt(NumberFormat childFormat) {
        if (childFormat == null) {
            childFormat = NumberFormat.getInstance();
        }
        this.childFormat = childFormat;
    }

    @Override
    public AttributedCharacterIterator formatToCharacterIterator(Object obj) {
        if (obj == null)
            return new AttributedString("").getIterator();
        return childFormat.formatToCharacterIterator(obj);
    }

    @Override
    public StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos) {
        if (obj == null)
            return new StringBuffer("");
        return childFormat.format(obj, toAppendTo, pos);
    }

    @Override
    public Number parse(String source, ParsePosition pos) {
        if (source == null) {
            pos.setIndex(1); // otherwise Format thinks parse failed
            return null;
        }
        if (source.trim().equals("")) {
            pos.setIndex(1); // otherwise Format thinks parse failed
            return null;
        }
        Number val = childFormat.parse(source, pos);
        /*
         * The default behaviour of Format objects is to keep parsing as long as
         * they encounter valid data. By for table editing we don't want
         * trailing bad data to be considered a "valid value". So set the index
         * to 0 so that the parse(Object) method knows that we had an error.
         */
        if (pos.getIndex() != source.length()) {
            pos.setErrorIndex(pos.getIndex());
            pos.setIndex(0);
        }
        return val;
    }

    @Override
    public StringBuffer format(double number, StringBuffer toAppendTo,
            FieldPosition pos) {
        return childFormat.format(number, toAppendTo, pos);
    }

    @Override
    public StringBuffer format(long number, StringBuffer toAppendTo,
            FieldPosition pos) {
        return childFormat.format(number, toAppendTo, pos);
    }

}
