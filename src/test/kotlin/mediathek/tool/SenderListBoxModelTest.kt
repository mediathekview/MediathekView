package mediathek.tool

import mediathek.filmlisten.FilmCatalog
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.DefaultComboBoxModel

internal class SenderListBoxModelTest {
    @Test
    fun `provided senders are sorted read-only catalog data`() {
        val senders = SenderListBoxModel.providedSenders

        assertEquals(senders.sortedWith(GermanStringSorter), senders)
        assertEquals(senders.size, senders.toSet().size)
        assertSame(senders, FilmCatalog().allSenders)
    }

    @Test
    fun `sender combo box uses standard model with empty first choice`() {
        val model = SenderListComboBoxModel(listOf("ARD", "ZDF"))

        assertInstanceOf(DefaultComboBoxModel::class.java, model)
        assertEquals(listOf("", "ARD", "ZDF"), model.elements())
        assertEquals("", model.selectedItem)
    }

    private fun DefaultComboBoxModel<String>.elements(): List<String> =
        (0 until size).map(::getElementAt)
}
