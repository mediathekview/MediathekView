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
