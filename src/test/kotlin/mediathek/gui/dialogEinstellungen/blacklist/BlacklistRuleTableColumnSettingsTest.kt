package mediathek.gui.dialogEinstellungen.blacklist

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import mediathek.daten.blacklist.ListeBlacklist
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import javax.swing.JTable

class BlacklistRuleTableColumnSettingsTest {
    @Test
    fun restoresColumnWidthAndPosition() {
        val store = InMemoryColumnStateStore(
            """
            {
              "columns": [
                {"id":"aktiv","width":55,"position":1},
                {"id":"Sender","width":120,"position":0}
              ]
            }
            """.trimIndent(),
        )
        val table = blacklistTable()

        BlacklistRuleTableColumnSettings(table, store).restore()

        assertEquals(BlacklistRuleTableModel.BLACKLIST_SENDER, table.convertColumnIndexToModel(0))
        assertEquals(
            55,
            table.columnModel.getColumn(table.convertColumnIndexToView(BlacklistRuleTableModel.BLACKLIST_ACTIVE)).width,
        )
        assertEquals(
            120,
            table.columnModel.getColumn(table.convertColumnIndexToView(BlacklistRuleTableModel.BLACKLIST_SENDER)).width,
        )
    }

    @Test
    fun savesColumnWidthAndPosition() {
        val store = InMemoryColumnStateStore()
        val table = blacklistTable()
        val settings = BlacklistRuleTableColumnSettings(table, store)
        table.columnModel.moveColumn(table.convertColumnIndexToView(BlacklistRuleTableModel.BLACKLIST_ACTIVE), 1)
        table.columnModel.getColumn(table.convertColumnIndexToView(BlacklistRuleTableModel.BLACKLIST_ACTIVE)).width = 88

        settings.save()

        val activeColumn = savedColumns(store.state).first { it["id"]?.jsonPrimitive?.content == "aktiv" }
        assertEquals("88", activeColumn["width"]?.jsonPrimitive?.content)
        assertEquals("1", activeColumn["position"]?.jsonPrimitive?.content)
    }

    private fun blacklistTable(): JTable =
        JTable(BlacklistRuleTableModel(ListeBlacklist()))

    private fun savedColumns(state: String) =
        Json.parseToJsonElement(state).jsonObject["columns"]!!.jsonArray.map { it.jsonObject }

    private class InMemoryColumnStateStore(
        var state: String = "",
    ) : BlacklistRuleTableColumnStateStore {
        override fun read(): String = state

        override fun write(state: String) {
            this.state = state
        }
    }
}
