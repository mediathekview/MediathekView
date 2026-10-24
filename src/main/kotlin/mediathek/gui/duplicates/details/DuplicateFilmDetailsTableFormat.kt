package mediathek.gui.duplicates.details

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.daten.DatenFilm
import mediathek.i18n.UiMessages

class DuplicateFilmDetailsTableFormat : AdvancedTableFormat<DatenFilm> {
    override fun getColumnClass(column: Int): Class<*> = String::class.java

    override fun getColumnComparator(column: Int): Comparator<*>? = when (column) {
        3, 4 -> null
        else -> naturalOrder<String>()
    }

    override fun getColumnCount(): Int = 7

    override fun getColumnName(column: Int): String = when (column) {
        0 -> UiMessages.text("duplicates.sender")
        1 -> UiMessages.text("duplicates.topic")
        2 -> UiMessages.text("duplicates.title")
        3 -> UiMessages.text("duplicates.date")
        4 -> UiMessages.text("duplicates.time")
        5 -> UiMessages.text("duplicates.hq")
        6 -> "URL"
        else -> "X$column"
    }

    override fun getColumnValue(baseObject: DatenFilm, column: Int): Any = when (column) {
        0 -> baseObject.sender
        1 -> baseObject.thema
        2 -> baseObject.title
        3 -> baseObject.sendeDatum
        4 -> baseObject.sendeZeit
        5 -> baseObject.highQualityUrl
        6 -> baseObject.urlNormalQuality
        else -> "XX"
    }
}
