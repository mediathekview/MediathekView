/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.filter

import java.util.*

/**
 * Matches a configured subtext against the start of an input string without regard to case.
 *
 * @author James Lemieux
 */
internal open class StartsWithCaseInsensitiveTextSearchStrategy : AbstractTextSearchStrategy() {
    private var indexOfStrategy: IndexOfStrategy? = null

    override fun setSubtext(subtext: String) {
        indexOfStrategy = if (subtext.length == 1) {
            SingleCharacterIndexOfStrategy(subtext[0])
        } else {
            MultiCharacterIndexOfStrategy(subtext)
        }
    }

    override fun indexOf(text: String): Int {
        val strategy = checkNotNull(indexOfStrategy) {
            "setSubtext must be called with a valid value before this method can operate"
        }
        return strategy.indexOf(text)
    }

    private fun interface IndexOfStrategy {
        fun indexOf(text: String): Int
    }

    private inner class SingleCharacterIndexOfStrategy(character: Char) : IndexOfStrategy {
        private val upperCase = character.uppercaseChar()
        private val lowerCase = character.lowercaseChar()

        override fun indexOf(text: String): Int {
            if (text.isEmpty()) return -1

            val character = map(text[0])
            return if (character == upperCase || character == lowerCase) 0 else -1
        }
    }

    private inner class MultiCharacterIndexOfStrategy(prefix: String) : IndexOfStrategy {
        private val subtextLength = prefix.length
        private val subtextCharsUpper = prefix.uppercase(Locale.getDefault()).toCharArray()
        private val subtextCharsLower = prefix.lowercase(Locale.getDefault()).toCharArray()

        override fun indexOf(text: String): Int {
            if (text.length < subtextLength) return -1

            for (index in 0 until subtextLength) {
                val character = map(text[index])
                if (subtextCharsLower[index] != character && subtextCharsUpper[index] != character) return -1
            }
            return 0
        }
    }
}
