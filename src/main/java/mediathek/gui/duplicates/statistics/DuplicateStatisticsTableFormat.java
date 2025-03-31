package mediathek.gui.duplicates.statistics;

import ca.odell.glazedlists.gui.AdvancedTableFormat;
import mediathek.gui.duplicates.FilmStatistics;

import java.util.Comparator;

public class DuplicateStatisticsTableFormat implements AdvancedTableFormat<FilmStatistics> {

    @Override
    public int getColumnCount() {
        return 2;
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case 0 -> "Sender";
            case 1 -> "Anzahl";
            default -> null;
        };
    }

    @Override
    public Object getColumnValue(FilmStatistics stats, int column) {
        return switch (column) {
            case 0 -> stats.sender();
            case 1 -> stats.count();
            default -> null;
        };
    }

    @Override
    public Class getColumnClass(int column) {
        return switch (column) {
            case 0 -> String.class;
            case 1 -> Long.class;
            default -> null;
        };
    }

    @Override
    public Comparator getColumnComparator(int column) {
        switch (column) {
            case 0 -> {
                return (Comparator<String>) String::compareTo;
            }
            case 1 -> {
                return (Comparator<Long>) Long::compareTo;
            }
        }
        return null;
    }
}
