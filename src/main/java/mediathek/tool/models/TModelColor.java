package mediathek.tool.models;

import mediathek.config.MVColor;
import mediathek.tool.MVC;

public class TModelColor extends NonEditableTableModel {
    public TModelColor(Object[][] data) {
        super(data, new String[]{"Beschreibung", "Farbe"});
    }

    @Override
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> result;
        if (columnIndex == MVColor.MVC_COLOR) {
            result = MVC.class;
        } else {
            result = String.class;
        }
        return result;
    }
}
