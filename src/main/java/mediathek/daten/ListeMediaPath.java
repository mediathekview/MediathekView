package mediathek.daten;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
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

    public void addObjectData(DefaultTableModel model) {
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
