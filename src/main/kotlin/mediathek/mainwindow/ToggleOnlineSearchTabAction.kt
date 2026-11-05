package mediathek.mainwindow

import mediathek.config.application.ApplicationConfiguration
import javax.swing.JTabbedPane
import mediathek.i18n.UiMessages

private val ACTION_TITLE get() = UiMessages.text("tab.onlineSearch.toggle")
private const val PREFERRED_INSERT_INDEX = 2

class ToggleOnlineSearchTabAction(
    tabbedPane: JTabbedPane,
    onlineSearchTab: MainWindowTab,
) : ToggleOptionalTabAction(
    tabbedPane,
    onlineSearchTab,
    ACTION_TITLE,
    { ApplicationConfiguration.getInstance().onlineSearchTabVisible = it },
    PREFERRED_INSERT_INDEX,
)
