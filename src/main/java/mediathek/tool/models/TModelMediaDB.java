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
