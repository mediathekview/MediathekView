package mediathek.gui.duplicates.statistics

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.gui.duplicates.FilmStatistics

class DuplicateStatisticsTableFormat : AdvancedTableFormat<FilmStatistics> {
    override fun getColumnCount(): Int = 2

    override fun getColumnName(column: Int): String? = when (column) {
        0 -> "Sender"
        1 -> "Anzahl"
        else -> null
    }

    override fun getColumnValue(stats: FilmStatistics, column: Int): Any? = when (column) {
        0 -> stats.sender
        1 -> stats.count
        else -> null
    }

    override fun getColumnClass(column: Int): Class<*>? = when (column) {
        0 -> String::class.java
        1 -> Long::class.java
        else -> null
    }

    override fun getColumnComparator(column: Int): Comparator<*>? = when (column) {
        0 -> naturalOrder<String>()
        1 -> naturalOrder<Long>()
        else -> null
    }
}
