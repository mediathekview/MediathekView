/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

/** Case-insensitive strategy optimized for a single-character subtext. */
internal open class SingleCharacterCaseInsensitiveTextSearchStrategy : AbstractTextSearchStrategy() {
    private var subtextCharLower = '\u0000'
    private var subtextCharUpper = '\u0000'
    private var subtextInitialized = false

    override fun setSubtext(subtext: String) {
        require(subtext.length == 1) {
            "subtext ($subtext) must contain a single character"
        }

        val character = subtext[0]
        subtextCharLower = character.lowercaseChar()
        subtextCharUpper = character.uppercaseChar()
        subtextInitialized = true
    }

    override fun indexOf(text: String): Int {
        check(subtextInitialized) {
            "setSubtext must be called with a valid value before this method can operate"
        }

        for (index in text.indices) {
            val character = map(text[index])
            if (character == subtextCharLower || character == subtextCharUpper) return index
        }
        return -1
    }
}
