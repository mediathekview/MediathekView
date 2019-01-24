/*
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool.models;

import mSearch.tool.Datum;
import mediathek.daten.DatenAbo;

@SuppressWarnings("serial")
public class TModelAbo extends TModel {
    public TModelAbo(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> result;
        switch (columnIndex) {
            case DatenAbo.ABO_NR:
            case DatenAbo.ABO_MINDESTDAUER:
                result = Integer.class;
                break;

            case DatenAbo.ABO_DOWN_DATUM:
                result = Datum.class;
                break;

            default:
                result = String.class;
                break;
        }
        return result;
    }
}
