package mediathek.controller.history

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class MVUsedUrlTest {

    @Test
    fun putzen() {
        val testStr = "g|a  |###|  b"
        val expected = "ga  ###  b"
        val result = MVUsedUrl.putzen(testStr)
        assertEquals(expected, result)
    }
}
