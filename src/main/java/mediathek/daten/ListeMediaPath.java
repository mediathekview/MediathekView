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

import mediathek.tool.TModel;

import javax.swing.*;
import java.util.LinkedList;

@SuppressWarnings("serial")
public class ListeMediaPath extends LinkedList<DatenMediaPath> {
    public boolean addSave(DatenMediaPath dmp) {
        while (countSave() > 10) {
            removeFirstSave();
        }
        if (!containSave(dmp)) {
            return add(dmp);
        } else
            return false;
    }

    public void addObjectData(TModel model) {
        model.setRowCount(0);
        this.stream().filter(datenMediaPath -> !datenMediaPath.savePath())
                .forEach(datenMediaPath -> model.addRow(datenMediaPath.arr));
    }

    public DefaultComboBoxModel<String> getComboModel() {
        DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>();
        this.stream().filter(DatenMediaPath::savePath)
                .forEach(datenMediaPath -> model.addElement(datenMediaPath.arr[DatenMediaPath.MEDIA_PATH_PATH]));
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
