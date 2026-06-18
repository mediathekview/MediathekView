package mediathek.mainwindow

import mediathek.gui.actions.QuitAction
import mediathek.gui.actions.ShowAboutAction
import javax.swing.Action
import javax.swing.JMenu

interface MainWindowMenuPolicy {
    val supportsFontMenu: Boolean

    fun addSettingsItem(fileMenu: JMenu, settingsAction: Action)

    fun addQuitItem(fileMenu: JMenu, owner: MediathekGui)

    fun addHelpTail(helpMenu: JMenu)
}

object DefaultMainWindowMenuPolicy : MainWindowMenuPolicy {
    override val supportsFontMenu: Boolean = true

    override fun addSettingsItem(fileMenu: JMenu, settingsAction: Action) {
        fileMenu.addSeparator()
        fileMenu.add(settingsAction)
    }

    override fun addQuitItem(fileMenu: JMenu, owner: MediathekGui) {
        fileMenu.addSeparator()
        fileMenu.add(QuitAction(owner))
    }

    override fun addHelpTail(helpMenu: JMenu) {
        helpMenu.addSeparator()
        helpMenu.add(ShowAboutAction())
    }
}

object MacMainWindowMenuPolicy : MainWindowMenuPolicy {
    override val supportsFontMenu: Boolean = false

    override fun addSettingsItem(fileMenu: JMenu, settingsAction: Action) = Unit

    override fun addQuitItem(fileMenu: JMenu, owner: MediathekGui) = Unit

    override fun addHelpTail(helpMenu: JMenu) = Unit
}
