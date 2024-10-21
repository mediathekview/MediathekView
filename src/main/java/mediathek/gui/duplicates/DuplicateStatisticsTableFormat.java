package mediathek.gui.duplicates;

import ca.odell.glazedlists.gui.TableFormat;

public class DuplicateStatisticsTableFormat implements TableFormat<DuplicateStatistics> {

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
}
