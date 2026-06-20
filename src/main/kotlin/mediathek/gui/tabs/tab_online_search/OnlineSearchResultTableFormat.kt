package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.tool.GermanStringSorter
import org.apache.commons.lang3.time.DurationFormatUtils
import java.time.Duration
import java.time.LocalDate
import java.time.format.DateTimeFormatter

class OnlineSearchResultTableFormat : AdvancedTableFormat<OnlineSearchResult> {
    override fun getColumnCount(): Int = COLUMN_COUNT

    override fun getColumnName(column: Int): String = when (column) {
        SENDER -> "Sender"
        TOPIC -> "Thema"
        TITLE -> "Titel"
        DATE -> "Datum"
        DURATION -> "Dauer"
        WEBSITE -> "Website"
        else -> throw IndexOutOfBoundsException("Unknown online search column: $column")
    }

    override fun getColumnValue(baseObject: OnlineSearchResult, column: Int): Any = when (column) {
        SENDER -> baseObject.sender
        TOPIC -> baseObject.displayTopic
        TITLE -> baseObject.title
        DATE -> baseObject.broadcastTime?.format(DATE_FORMATTER).orEmpty()
        DURATION -> baseObject.duration?.let { DurationFormatUtils.formatDuration(it.toMillis(), "HH:mm:ss", true) }.orEmpty()
        WEBSITE -> baseObject.websiteUrl
        else -> throw IndexOutOfBoundsException("Unknown online search column: $column")
    }

    override fun getColumnClass(column: Int): Class<*> = String::class.java

    override fun getColumnComparator(column: Int): Comparator<*>? = when (column) {
        TOPIC -> GermanStringSorter
        TITLE -> GermanStringSorter
        DATE -> DATE_COMPARATOR
        DURATION -> DURATION_COMPARATOR
        else -> null
    }

    companion object {
        const val SENDER = 0
        const val TOPIC = 1
        const val TITLE = 2
        const val DATE = 3
        const val DURATION = 4
        const val WEBSITE = 5
        private const val COLUMN_COUNT = 6
        private val DATE_FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        private val DATE_COMPARATOR: Comparator<String> = compareBy { value ->
            value.takeIf { it.isNotBlank() }?.let { LocalDate.parse(it, DATE_FORMATTER) }
        }
        private val DURATION_COMPARATOR: Comparator<String> = compareBy { value ->
            value.takeIf { it.isNotBlank() }?.split(':')?.let { parts ->
                Duration.ofHours(parts[0].toLong())
                    .plusMinutes(parts[1].toLong())
                    .plusSeconds(parts[2].toLong())
            }
        }
    }
}
