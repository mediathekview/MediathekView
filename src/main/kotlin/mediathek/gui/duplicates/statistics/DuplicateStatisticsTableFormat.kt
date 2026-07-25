package mediathek.gui.duplicates.statistics

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.gui.duplicates.FilmStatistics

class DuplicateStatisticsTableFormat : AdvancedTableFormat<FilmStatistics> {
    override fun getColumnCount(): Int = 2

    override fun getColumnName(column: Int): String = when (column) {
        0 -> "Sender"
        1 -> "Anzahl"
        else -> throw IndexOutOfBoundsException(column)
    }

    override fun getColumnValue(baseObject: FilmStatistics, column: Int): Any? = when (column) {
        0 -> baseObject.sender
        1 -> baseObject.count
        else -> null
    }

    override fun getColumnClass(column: Int): Class<*> = when (column) {
        0 -> String::class.java
        1 -> Long::class.java
        else -> throw IndexOutOfBoundsException(column)
    }

    override fun getColumnComparator(column: Int): Comparator<*>? = when (column) {
        0 -> naturalOrder<String>()
        1 -> naturalOrder<Long>()
        else -> null
    }
}
