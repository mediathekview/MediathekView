package mediathek.tool

/**
 * This class check if titel contains specific keywords.
 */
class TrailerTeaserChecker {
    fun check(titel: String): Boolean {
        val lTitel = titel.toLowerCase()
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