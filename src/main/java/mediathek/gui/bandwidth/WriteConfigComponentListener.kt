package mediathek.gui.bandwidth

import org.apache.commons.configuration2.Configuration
import org.apache.commons.configuration2.sync.LockMode
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import javax.swing.JDialog

internal class WriteConfigComponentListener(private val config: Configuration, private val dialog: JDialog) :
    ComponentAdapter() {
    override fun componentResized(e: ComponentEvent) {
        try {
            config.lock(LockMode.WRITE)
            val size = dialog.size
            config.setProperty(BandwidthDialog.CONFIG_WIDTH, size.width)
            config.setProperty(BandwidthDialog.CONFIG_HEIGHT, size.height)
        }
        finally {
            config.unlock(LockMode.WRITE)
        }
    }

    override fun componentMoved(e: ComponentEvent) {
        try {
            config.lock(LockMode.WRITE)
            val location = dialog.location
            config.setProperty(BandwidthDialog.CONFIG_X, location.x)
            config.setProperty(BandwidthDialog.CONFIG_Y, location.y)
        }
        finally {
            config.unlock(LockMode.WRITE)
        }
    }
}