package mediathek.gui.tabs.tab_film

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class SearchFieldDataTest {

    @Test
    fun searchThroughDescriptions() {
        var data = SearchFieldData("", SearchControlFieldMode.IRGENDWO)
        assertTrue(data.searchThroughDescriptions())

        data = SearchFieldData("", SearchControlFieldMode.THEMA_TITEL);
        assertFalse(data.searchThroughDescriptions())
    }

    @Test
    fun isEmpty() {
        var data = SearchFieldData("", SearchControlFieldMode.THEMA_TITEL)
        assertTrue(data.isEmpty())

        data = SearchFieldData("test", SearchControlFieldMode.THEMA_TITEL)
        assertFalse(data.isEmpty())
    }

    @Test
    fun evaluateThemaTitel() {
        var data = SearchFieldData("Axel,Prahl", SearchControlFieldMode.THEMA_TITEL)
        var res = data.evaluateThemaTitel()
        assertEquals(res[0], "axel")
        assertEquals(res[1], "prahl")

        data = SearchFieldData("Axel Prahl", SearchControlFieldMode.THEMA_TITEL)
        res = data.evaluateThemaTitel()
        assertEquals(res[0], "axel prahl")
    }

    @Test
    fun searchFieldText() {
        val data = SearchFieldData("testStr", SearchControlFieldMode.THEMA_TITEL)
        assertEquals(data.searchFieldText, "testStr");
    }
}