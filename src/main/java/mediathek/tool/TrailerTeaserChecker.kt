package mediathek.tool

/**
 * This class check if titel contains specific keywords.
 */
class TrailerTeaserChecker {
    /**
     * Check if a string might belong to a trailer, teaser, etc.
     */
    fun check(content: String): Boolean {
        val lTitel = content.toLowerCase()
        return containsTrailer(lTitel) || containsTeaser(lTitel) || containsVorschau(lTitel)
    }

    private fun containsTrailer(titel: String): Boolean {
        return titel.contains("trailer")
    }

    private fun containsTeaser(titel: String): Boolean {
        return titel.contains("teaser")
    }

    private fun containsVorschau(titel: String): Boolean {
        return titel.contains("vorschau")
    }
}