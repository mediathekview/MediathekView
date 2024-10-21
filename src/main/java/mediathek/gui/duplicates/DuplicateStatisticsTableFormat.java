package mediathek.gui.duplicates;

import ca.odell.glazedlists.gui.AdvancedTableFormat;

import java.util.Comparator;

public class DuplicateStatisticsTableFormat implements AdvancedTableFormat<DuplicateStatistics> {

    @Override
    public int getColumnCount() {
        return 2;
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case 0 -> "Sender";
            case 1 -> "Anzahl Duplikate";
            default -> null;
        };
    }

    @Override
    public Object getColumnValue(DuplicateStatistics stats, int column) {
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
