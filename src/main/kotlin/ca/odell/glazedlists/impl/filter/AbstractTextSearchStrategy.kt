/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

/** Shared optional character mapping for text-search strategies. */
internal abstract class AbstractTextSearchStrategy : TextSearchStrategy {
    private var characterMap: CharArray? = null

    override fun setCharacterMap(charMap: CharArray?) {
        characterMap = charMap
    }

    /** Maps [character] when the configured map contains an entry for it. */
    protected open fun map(character: Char): Char {
        val currentMap = characterMap
        return if (currentMap != null && character.code < currentMap.size) {
            currentMap[character.code]
        } else {
            character
        }
    }
}
