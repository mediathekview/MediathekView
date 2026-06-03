package mediathek.tool

object HtmlUtils {
    @JvmStatic
    fun escapeHtml(text: String): String = buildString(text.length) {
        for (character in text) {
            append(escapeHtmlCharacter(character) ?: character)
        }
    }

    fun escapeHtmlCharacter(character: Char): String? = when (character) {
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        else -> null
    }
}
