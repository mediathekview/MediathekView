package mediathek.gui.actions

import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.BlacklistChangedEvent
import mediathek.swing.IconUtils
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import org.kordamp.ikonli.materialdesign2.MaterialDesignL
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.SwingUtilities

class ToggleBlacklistAction : AbstractAction() {
    private val enabledIcon: FontIcon = IconUtils.windowBarSpecificToolbarIcon(MaterialDesignL.LIST_STATUS)
    private val disabledIcon: FontIcon = IconUtils.windowBarSpecificToolbarIcon(MaterialDesignL.LIST_STATUS, Color.RED)
    private var blacklistIsOn: Boolean = ApplicationConfiguration.getInstance().isBlacklistEnabled

    init {
        setupState()
        MessageBus.messageBus.subscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleBlacklistChangedEvent(event: BlacklistChangedEvent) {
        SwingUtilities.invokeLater {
            blacklistIsOn = ApplicationConfiguration.getInstance().isBlacklistEnabled
            setupState()
        }
    }

    private fun setupState() {
        if (blacklistIsOn) {
            putValue(NAME, "Blacklist ausschalten")
            putValue(SHORT_DESCRIPTION, "Blacklist ausschalten")
            putValue(SMALL_ICON, enabledIcon)
        } else {
            putValue(NAME, "Blacklist einschalten")
            putValue(SHORT_DESCRIPTION, "Blacklist einschalten")
            putValue(SMALL_ICON, disabledIcon)
        }
    }

    override fun actionPerformed(event: ActionEvent?) {
        blacklistIsOn = !blacklistIsOn

        ApplicationConfiguration.getInstance().isBlacklistEnabled = blacklistIsOn
        Daten.getInstance().listeBlacklist.filterListe()
        MessageBus.messageBus.publishAsync(BlacklistChangedEvent())
    }
}
