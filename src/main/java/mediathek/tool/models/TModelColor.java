package mediathek.tool.models;

import mediathek.config.MVColor;
import mediathek.tool.MVC;

@SuppressWarnings("serial")
public class TModelColor extends TModel {
    public TModelColor(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
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
