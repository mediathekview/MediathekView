package mediathek.gui.tabs.tab_film

import mediathek.javaswing.filterpanel.SearchControlFieldModeSwing
import mediathek.tool.Filter
import java.util.regex.Pattern

@JvmRecord
data class SearchFieldData(val searchFieldText: String, val searchMode: SearchControlFieldModeSwing) {
    fun searchThroughDescriptions(): Boolean {
        return searchMode == SearchControlFieldModeSwing.IRGENDWO
    }

    fun isEmpty(): Boolean {
        return searchFieldText.isEmpty()
    }

    fun evaluateThemaTitel(): Array<String> {
        return if (Filter.isPattern(searchFieldText)) {
            arrayOf(searchFieldText)
        } else {
            Pattern.compile(",")
                .splitAsStream(searchFieldText).map { s: String -> s.lowercase() }
                .toArray { size -> arrayOfNulls<String>(size) }
        }
    }
}
