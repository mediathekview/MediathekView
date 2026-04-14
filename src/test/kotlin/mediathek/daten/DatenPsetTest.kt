package mediathek.daten

import mediathek.config.Daten
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class DatenPsetTest {

    @Test
    fun parsingAbspielenFlagDoesNotResetGlobalPlaybackSelection() {
        val listePset = Daten.getInstance().listePset
        val originalState = ListePset()
        originalState.addAll(listePset)
        try {
            listePset.clear()

            val active = DatenPset("active")
            val other = DatenPset("other")
            listePset.add(active)
            listePset.add(other)
            listePset.activateAsPlayer(active)

            val temporary = DatenPset()
            temporary[DatenPset.PROGRAMMSET_IST_ABSPIELEN] = java.lang.Boolean.TRUE.toString()

            assertTrue(active.istAbspielen())
            assertFalse(other.istAbspielen())
            assertTrue(temporary.istAbspielen())
        } finally {
            listePset.clear()
            listePset.addAll(originalState)
        }
    }

    @Test
    fun directAddKeepsOnlyOnePlaybackSelection() {
        val listePset = ListePset()

        val first = DatenPset("first")
        first.setAbspielen(true)
        listePset.add(first)

        val second = DatenPset("second")
        second.setAbspielen(true)
        listePset.add(second)

        assertFalse(first.istAbspielen())
        assertTrue(second.istAbspielen())
        assertEquals(second, listePset.psetAbspielen)
    }

    @Test
    fun addAllKeepsOnlyLastPlaybackSelectionFromIncomingCollection() {
        val listePset = ListePset()

        val existing = DatenPset("existing")
        existing.setAbspielen(true)
        listePset.add(existing)

        val firstIncoming = DatenPset("firstIncoming")
        firstIncoming.setAbspielen(true)
        val secondIncoming = DatenPset("secondIncoming")
        secondIncoming.setAbspielen(true)

        listePset.addAll(listOf(firstIncoming, secondIncoming))

        assertFalse(existing.istAbspielen())
        assertFalse(firstIncoming.istAbspielen())
        assertTrue(secondIncoming.istAbspielen())
        assertEquals(secondIncoming, listePset.psetAbspielen)
    }
}
