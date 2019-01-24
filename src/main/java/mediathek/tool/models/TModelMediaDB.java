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

import mediathek.daten.DatenMediaDB;
import mediathek.tool.MVMediaDBFileSize;

@SuppressWarnings("serial")
public class TModelMediaDB extends TModel {

    public TModelMediaDB(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> result;
        if (columnIndex == DatenMediaDB.MEDIA_DB_SIZE) {
            result = MVMediaDBFileSize.class;
        } else {
            result = String.class;
        }

        return result;
    }
}
