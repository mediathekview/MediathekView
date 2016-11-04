/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.DefaultComboBoxModel;
import mediathek.tool.TModel;

public class ListeMediaPath extends LinkedList<DatenMediaPath> {

    private static final long serialVersionUID = 1L;

    public boolean addSave(DatenMediaPath dmp) {
        while (countSave() > 10) {
            removeFirstSave();
        }
        if (!containSave(dmp)) {
            return add(dmp);
        }
        return false;
    }

    public void addObjectData(TModel model) {
        DatenMediaPath dmp;
        model.setRowCount(0);
        Iterator<DatenMediaPath> iterator = this.iterator();
        while (iterator.hasNext()) {
            dmp = iterator.next();
            if (!dmp.savePath()) {
                model.addRow(dmp.arr);
            }
        }
    }

    public DefaultComboBoxModel<String> getComboModel() {
        DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>();
        DatenMediaPath dmp;
        Iterator<DatenMediaPath> iterator = this.iterator();
        while (iterator.hasNext()) {
            dmp = iterator.next();
            if (dmp.savePath()) {
                model.addElement(dmp.arr[DatenMediaPath.MEDIA_PATH_PATH]);
            }
        }
        return model;
    }

    private boolean containSave(DatenMediaPath dm) {
        for (DatenMediaPath dmp : this) {
            if (dmp.savePath() && dmp.arr[DatenMediaPath.MEDIA_PATH_PATH].equals(dm.arr[DatenMediaPath.MEDIA_PATH_PATH])) {
                return true;
            }
        }
        return false;
    }

    private int countSave() {
        int ret = 0;
        for (DatenMediaPath dmp : this) {
            if (dmp.savePath()) {
                ++ret;
            }
        }
        return ret;
    }

    private void removeFirstSave() {
        for (DatenMediaPath dmp : this) {
            if (dmp.savePath()) {
                this.remove(dmp);
                return;
            }
        }
    }

}
