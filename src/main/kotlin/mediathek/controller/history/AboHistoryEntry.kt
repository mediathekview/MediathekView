package mediathek.controller.history

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

data class AboHistoryEntry(
    val date: LocalDate,
    val theme: String,
    val title: String,
    val url: String
) {
    val formattedDate: String
        get() = date.format(DATE_FORMATTER)

    companion object {
        private val DATE_FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")

        fun today(theme: String, title: String, url: String): AboHistoryEntry =
            AboHistoryEntry(LocalDate.now(), theme, title, url)

        fun parse(date: String, theme: String, title: String, url: String): AboHistoryEntry? {
            return try {
                AboHistoryEntry(LocalDate.parse(date, DATE_FORMATTER), theme, title, url)
            } catch (_: DateTimeParseException) {
                null
            }
        }
    }
}
