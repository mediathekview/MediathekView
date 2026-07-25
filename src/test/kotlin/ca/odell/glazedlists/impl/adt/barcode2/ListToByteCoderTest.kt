package ca.odell.glazedlists.impl.adt.barcode2

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class ListToByteCoderTest {
    @Test
    fun singleColorBitsMapToTheirIndices() {
        listOf<Byte>(1, 2, 4, 8, 16, 32, 64).forEachIndexed { index, color ->
            assertEquals(index, ListToByteCoder.colorAsIndex(color))
        }
    }

    @Test
    fun combinedOrMissingColorBitsAreRejected() {
        listOf<Byte>(0, 3, -128).forEach { color ->
            assertThrows(IllegalArgumentException::class.java) {
                ListToByteCoder.colorAsIndex(color)
            }
        }
    }

    @Test
    fun colorsAreCopiedAndRemainUnmodifiable() {
        val source = mutableListOf("red", null)
        val coder = ListToByteCoder(source)

        source[0] = "changed"

        assertEquals(listOf("red", null), coder.colors)
        assertEquals(2, coder.colorToByte(null).toInt())
        assertThrows(UnsupportedOperationException::class.java) {
            (coder.colors as MutableList<String?>).add("blue")
        }
    }

    @Test
    fun duplicateColorsAreRejected() {
        assertThrows(IllegalArgumentException::class.java) {
            ListToByteCoder(listOf("red", "red"))
        }
        assertThrows(IllegalArgumentException::class.java) {
            ListToByteCoder(listOf(null, null))
        }
    }

    @Test
    fun sevenColorsAreSupported() {
        val coder = ListToByteCoder((0..6).toList())

        assertEquals(0b01111111, coder.allColorsToByte().toInt())
    }

    @Test
    fun eighthColorIsRejected() {
        assertThrows(IllegalArgumentException::class.java) {
            ListToByteCoder((0..7).toList())
        }
    }

    @Test
    fun unknownColorsAreRejectedInsteadOfSilentlyEncodingAsZero() {
        val coder = ListToByteCoder(listOf("red", "blue"))

        assertThrows(IllegalArgumentException::class.java) {
            coder.colorToByte("green")
        }
        assertThrows(IllegalArgumentException::class.java) {
            coder.colorsToByte(listOf("red", "green"))
        }
    }
}
