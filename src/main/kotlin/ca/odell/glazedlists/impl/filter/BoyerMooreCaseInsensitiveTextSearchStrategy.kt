/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl.filter

private const val CHARACTER_CACHE_SIZE = 256

/**
 * Implements a simple version of the Boyer-Moore text-searching algorithm,
 * generally considered to be the fastest known text-searching algorithm.
 *
 * @author James Lemieux
 */
internal open class BoyerMooreCaseInsensitiveTextSearchStrategy : AbstractTextSearchStrategy() {
    private var subtextLength = 0
    private var lastSubtextIndex = 0
    private var subtextChars: CharArray? = null
    private val shiftTable = IntArray(CHARACTER_CACHE_SIZE)

    /**
     * Builds a shortened Boyer-Moore shift table. The table normally contains
     * an entry for every character in the text-search alphabet. Since this
     * strategy covers all Unicode characters, the complete table would be
     * large. Instead, each character is mapped to one of 256 entries. Any
     * resulting collisions can only make the search suboptimal, not incorrect.
     */
    override fun setSubtext(subtext: String) {
        subtextLength = subtext.length
        lastSubtextIndex = subtextLength - 1
        val foldedSubtext = CharArray(subtextLength) { index -> foldCaseAt(subtext, index) }
        subtextChars = foldedSubtext

        shiftTable.fill(subtextLength)
        for (index in 0 until lastSubtextIndex) {
            shiftTable[foldedSubtext[index].code % CHARACTER_CACHE_SIZE] = lastSubtextIndex - index
        }
    }

    override fun indexOf(text: String): Int {
        val currentSubtextChars = checkNotNull(subtextChars) {
            "setSubtext must be called with a valid value before this method can operate"
        }

        var textPosition = lastSubtextIndex
        var textCharacter = ' '
        val textLength = text.length

        while (textPosition < textLength) {
            var subtextPosition = lastSubtextIndex

            if (subtextPosition >= 0) {
                textCharacter = foldMappedCaseAt(text, textPosition)

                while (subtextPosition >= 0 && currentSubtextChars[subtextPosition] == textCharacter) {
                    textPosition--
                    if (textPosition != -1) {
                        textCharacter = foldMappedCaseAt(text, textPosition)
                    }
                    // Work around https://bugs.openjdk.org/browse/JDK-8054478.
                    subtextPosition--
                }
            }

            if (subtextPosition == -1) return textPosition + 1

            textPosition += maxOf(
                shiftTable[textCharacter.code % CHARACTER_CACHE_SIZE],
                subtextLength - subtextPosition,
            )
        }

        return -1
    }

    private fun foldCaseAt(value: String, index: Int): Char = foldCaseAt(value, index, applyCharacterMap = false)

    private fun foldMappedCaseAt(value: String, index: Int): Char =
        foldCaseAt(value, index, applyCharacterMap = true)

    private fun foldCaseAt(value: String, index: Int, applyCharacterMap: Boolean): Char {
        val character = characterAt(value, index, applyCharacterMap)

        if (character.isHighSurrogate() && index + 1 < value.length) {
            val lowSurrogate = characterAt(value, index + 1, applyCharacterMap)
            if (lowSurrogate.isLowSurrogate()) {
                val foldedCodePoint = foldCase(Character.toCodePoint(character, lowSurrogate))
                if (Character.isSupplementaryCodePoint(foldedCodePoint)) {
                    return Character.highSurrogate(foldedCodePoint)
                }
            }
        } else if (character.isLowSurrogate() && index > 0) {
            val highSurrogate = characterAt(value, index - 1, applyCharacterMap)
            if (highSurrogate.isHighSurrogate()) {
                val foldedCodePoint = foldCase(Character.toCodePoint(highSurrogate, character))
                if (Character.isSupplementaryCodePoint(foldedCodePoint)) {
                    return Character.lowSurrogate(foldedCodePoint)
                }
            }
        }

        return foldCase(character.code).toChar()
    }

    private fun characterAt(value: String, index: Int, applyCharacterMap: Boolean): Char {
        val character = value[index]
        return if (applyCharacterMap) map(character) else character
    }
}

/** Applies the locale-independent code-point folding used by case-insensitive string comparison. */
private fun foldCase(codePoint: Int): Int = Character.toLowerCase(Character.toUpperCase(codePoint))
