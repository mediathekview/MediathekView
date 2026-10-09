/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

/** Locates a configured subtext within arbitrary text. */
internal interface TextSearchStrategy {
    /** Sets an optional character-normalization map used during comparisons. */
    fun setCharacterMap(charMap: CharArray?)

    /** Sets the subtext subsequently located by [indexOf]. */
    fun setSubtext(subtext: String)

    /** Returns the first index of the configured subtext in [text], or `-1`. */
    fun indexOf(text: String): Int

    /** Creates a prepared strategy implementation for a matching mode and filter. */
    @FunctionalInterface
    fun interface Factory {
        fun create(mode: Int, filter: String): TextSearchStrategy
    }
}
