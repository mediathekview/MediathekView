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

        switch (column) {
            case DatenAbo.ABO_NR:
                result = "Nr";
                break;

            case DatenAbo.ABO_EINGESCHALTET:
                result = "aktiv";
                break;

            case DatenAbo.ABO_NAME:
                result = "Name";
                break;

            case DatenAbo.ABO_SENDER:
                result = "Sender";
                break;

            case DatenAbo.ABO_THEMA:
                result = "Thema";
                break;

            case DatenAbo.ABO_TITEL:
                result = "Titel";
                break;

            case DatenAbo.ABO_THEMA_TITEL:
                result = "Thema-Titel";
                break;

            case DatenAbo.ABO_IRGENDWO:
                result = "Irgendwo";
                break;

            case DatenAbo.ABO_MINDESTDAUER:
                result = "Dauer";
                break;

            case DatenAbo.ABO_MIN:
                result = "min/max";
                break;

            case DatenAbo.ABO_ZIELPFAD:
                result = "Zielpfad";
                break;

            case DatenAbo.ABO_DOWN_DATUM:
                result = "letztes Abo";
                break;

            case DatenAbo.ABO_PSET:
                result = "Programmset";
                break;

            default:
                throw new IndexOutOfBoundsException("UNKNOWN COLUMN NAME: " + column);
        }

        return result;
    }
}
