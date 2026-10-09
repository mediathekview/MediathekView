package mediathek.daten

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class ListePsetTest {

    @Test
    fun hasAboProgramSetReflectsAboProgramSets() {
        val listePset = ListePset()

        assertFalse(listePset.hasAboProgramSet())

        listePset.add(DatenPset("player"))
        assertFalse(listePset.hasAboProgramSet())

        val aboSet = DatenPset("abo")
        aboSet.setAbo(true)
        listePset.add(aboSet)

        assertTrue(listePset.hasAboProgramSet())
    }

    @Test
    fun hasDownloadProgramSetReflectsDownloadProgramSets() {
        val listePset = ListePset()

        assertFalse(listePset.hasDownloadProgramSet())

        listePset.add(DatenPset("player"))
        assertFalse(listePset.hasDownloadProgramSet())

        val downloadSet = DatenPset("download")
        downloadSet.setSpeichern(true)
        listePset.add(downloadSet)

        assertTrue(listePset.hasDownloadProgramSet())
    }
}
