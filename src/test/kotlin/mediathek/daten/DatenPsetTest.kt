package mediathek.daten

import mediathek.config.Daten
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

internal class DatenPsetTest {
    private lateinit var daten: Daten

    @BeforeEach
    fun setUp() {
        daten = Daten()
    }

    @AfterEach
    fun tearDown() {
        daten.downloads.shutdown()
    }

    @Test
    fun parsingAbspielenFlagDoesNotResetGlobalPlaybackSelection() {
        val listePset = daten.programSets.list
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

    @Test
    fun psetArrayKeepsLegacyInfoUrlColumn() {
        val pset = DatenPset()
        pset[DatenPset.PROGRAMMSET_INFO_URL] = "https://example.invalid/info"

        val values = pset.toArray()

        assertEquals("Url Info", DatenPset.COLUMN_NAMES[DatenPset.PROGRAMMSET_INFO_URL])
        assertEquals("Info-URL", DatenPset.XML_NAMES[DatenPset.PROGRAMMSET_INFO_URL])
        assertEquals("https://example.invalid/info", values[DatenPset.PROGRAMMSET_INFO_URL])
    }

    @Test
    fun copyFromPreservesInfoUrlAndDefaultValues() {
        val values = Array(DatenPset.MAX_ELEM) { "" }
        values[DatenPset.PROGRAMMSET_NAME] = "Set"
        values[DatenPset.PROGRAMMSET_INFO_URL] = "https://example.invalid/info"

        val pset = DatenPset()
        pset.copyFrom(values)

        assertEquals("Set", pset.name)
        assertEquals("https://example.invalid/info", pset[DatenPset.PROGRAMMSET_INFO_URL])
        assertTrue(pset.isThemaAnlegen)
        assertFalse(pset.istAbspielen())
        assertFalse(pset.istSpeichern())
        assertFalse(pset.istButton())
        assertFalse(pset.istAbo())
        assertFalse(pset.shouldCreateInfofile())
        assertFalse(pset.shouldDownloadSubtitle())
        assertEquals(FilmResolution.Enum.NORMAL, pset.aufloesung)
    }
}
