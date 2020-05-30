package mediathek.tool.models;

import mediathek.daten.DatenAbo;
import mediathek.tool.Datum;

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

    @Override
    public String getColumnName(int column) {
        String result;
        if (column >= 0 || column < DatenAbo.MAX_ELEM) {
          result = DatenAbo.COLUMN_NAMES[column]; 
        }
        else {
          throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        }

        return result;
    }
}
