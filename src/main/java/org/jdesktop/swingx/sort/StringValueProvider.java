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
package org.jdesktop.swingx.sort;

import org.jdesktop.swingx.renderer.StringValue;

/**
 * Read-only repository for StringValues. This is meant to be shared by collection views
 * (in rendering a cell) and RowSorters/SortControllers/ComponentAdapters. <p>
 * 
 * Note: this is work-in-progress, related to re-enable WYSIWYM in sorting/filtering.
 * It's location and api is expected to change.
 * 
 * @author Jeanette Winzenburg
 */
public interface StringValueProvider {

    /**
     * Returns a StringValue to use for conversion of the cell content at row and column.
     * The converter is guaranteed to be not null, so implemenations are responsible for
     * a reasonable fall-back value always, f.i. if they have no converters registered of
     * if any or both of the row/column coordinate is "invalid" (f.i. -1) <p>
     * 
     * @param row the row of the cell in model coordinates
     * @param column the column of the cell in model coordinates
     * 
     * @return a StringValue to use for conversion, guaranteed to not null.
     */
    StringValue getStringValue(int row, int column);

}
