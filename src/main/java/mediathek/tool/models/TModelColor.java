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

import mediathek.config.MVColor;
import mediathek.tool.MVC;

@SuppressWarnings("serial")
public class TModelColor extends TModel {
    private final Class<?>[] types;

    public TModelColor(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
        types = new Class<?>[MVColor.MVC_MAX];
        for (int i = 0; i < MVColor.MVC_MAX; ++i) {
            if (i == MVColor.MVC_COLOR) {
                types[i] = MVC.class;
            } else {
                types[i] = String.class;
            }
        }
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        return types[columnIndex];
    }
}
