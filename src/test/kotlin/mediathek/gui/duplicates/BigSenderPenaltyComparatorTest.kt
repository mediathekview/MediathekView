package mediathek.gui.duplicates

import mediathek.daten.DatenFilm
import mediathek.tool.GermanStringSorter
import mediathek.tool.SenderListBoxModel
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class BigSenderPenaltyComparatorTest {

    @Test
    fun sortProvidedSenderList_movesArdAndZdfToEnd() {
        val inputFilms = SenderListBoxModel.providedSenderList
            .toList()
            .map(::createFilmWithSender)
            .sortedWith(BigSenderPenaltyComparator())

        val sortedSenders = inputFilms.map(DatenFilm::getSender)
        val senderCount = sortedSenders.size

        val senderSet = setOf("ARD", "ZDF")
        assertTrue(senderSet.contains(sortedSenders[senderCount - 1]))
        assertTrue(senderSet.contains(sortedSenders[senderCount - 2]))
        assertNotEquals(sortedSenders[senderCount - 1], sortedSenders[senderCount - 2])

        val expectedWithoutPenalty = SenderListBoxModel.providedSenderList
            .toList()
            .filter { sender -> sender != "ARD" && sender != "ZDF" }
            .sortedWith(GermanStringSorter)
        val actualWithoutPenalty = sortedSenders.subList(0, senderCount - 2)

        assertEquals(expectedWithoutPenalty, actualWithoutPenalty)
    }

    private fun createFilmWithSender(sender: String): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
        }
}
