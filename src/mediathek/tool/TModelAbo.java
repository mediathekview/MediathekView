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
package mediathek.tool;

import mediathek.daten.DatenAbo;

public class TModelAbo extends TModel {

    Class[] types;

    public TModelAbo(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
        types = new Class[DatenAbo.MAX_ELEM];
        for (int i = 0; i < DatenAbo.MAX_ELEM; ++i) {
            switch (i) {
                case DatenAbo.ABO_DOWN_DATUM_NR:
                    types[i] = Datum.class;
                    break;
                default:
                    types[i] = String.class;
            }
        }
    }

    @Override
    public Class getColumnClass(int columnIndex) {
        return types[columnIndex];
    }
//    @Override
//    public Class<?> getColumnClass(int columnIndex) {
//        switch (columnIndex) {
//            case DatenAbo.ABO_EINGESCHALTET_NR:
//            case DatenAbo.ABO_THEMA_EXAKT_NR:
//                return Boolean.class;
//            case DatenAbo.ABO_DOWN_DATUM_NR:
//                return Datum.class;
//            default:
//                return String.class;
//        }
//    }
}
