package mediathek.gui.duplicates.details;

import ca.odell.glazedlists.gui.AdvancedTableFormat;
import mediathek.daten.DatenFilm;

import java.util.Comparator;

public class DuplicateFilmDetailsTableFormat implements AdvancedTableFormat<DatenFilm> {

    @Override
    public Class getColumnClass(int column) {
        return String.class;
    }

    @Override
    public Comparator getColumnComparator(int column) {
        return switch (column) {
            case 3, 4 -> null;
            default -> (Comparator<String>) String::compareTo;
        };
    }

    @Override
    public int getColumnCount() {
        return 5;
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case 0 -> "Sender";
            case 1 -> "Thema";
            case 2 -> "Titel";
            case 3 -> "Datum";
            case 4 -> "Sendezeit";
            default -> "X" + column;
        };
    }

    @Override
    public Object getColumnValue(DatenFilm baseObject, int column) {
        return switch (column) {
            case 0 -> baseObject.getSender();
            case 1 -> baseObject.getThema();
            case 2 -> baseObject.getTitle();
            case 3 -> baseObject.getSendeDatum();
            case 4 -> baseObject.getSendeZeit();
            default -> "XX";
        };
    }
}
