package mediathek.gui.tabs.tab_film.search

import mediathek.tool.Filter
import java.util.regex.Pattern

data class SearchFieldData(val searchFieldText: String, val searchMode: SearchControlFieldMode) {
    fun searchThroughDescriptions(): Boolean {
        return searchMode == SearchControlFieldMode.IRGENDWO
    }

    fun isEmpty(): Boolean {
        return searchFieldText.isEmpty()
    }

    fun evaluateThemaTitel(): Array<String> {
        return if (Filter.isPattern(searchFieldText)) {
            arrayOf(searchFieldText)
        } else {
            COMMA_SPLIT.splitAsStream(searchFieldText).map { s: String -> s.lowercase() }
                .toArray { size -> arrayOfNulls<String>(size) }
        }
    }

    private companion object {
        private val COMMA_SPLIT = Pattern.compile(",")
    }
}
