package mediathek.gui.watchlist

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.SenderIconStyleChangedEvent
import mediathek.tool.MessageBus
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Test
import javax.swing.JTable
import javax.swing.table.DefaultTableModel

internal class WatchlistSenderCellRendererTest {
    @Test
    fun `sender renderer displays the configured sender icon`() {
        val configuration = ApplicationConfiguration.getInstance()
        val previousLocalSenderIcons = configuration.localSenderIcons
        try {
            configuration.localSenderIcons = true
            MessageBus.messageBus.publish(SenderIconStyleChangedEvent())
            val table = JTable(DefaultTableModel(arrayOf(arrayOf("ARD")), arrayOf("Sender"))).apply {
                rowHeight = 36
                columnModel.getColumn(0).width = 100
            }

            val renderer = WatchlistSenderCellRenderer()
            renderer.getTableCellRendererComponent(
                table,
                "ARD",
                isSelected = false,
                hasFocus = false,
                row = 0,
                column = 0,
            )

            assertNotNull(renderer.icon)
            assertEquals("", renderer.text)
        } finally {
            configuration.localSenderIcons = previousLocalSenderIcons
            MessageBus.messageBus.publish(SenderIconStyleChangedEvent())
        }
    }
}
