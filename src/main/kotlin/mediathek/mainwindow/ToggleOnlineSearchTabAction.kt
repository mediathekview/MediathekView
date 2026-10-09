package mediathek.mainwindow

import mediathek.config.application.ApplicationConfiguration
import javax.swing.JTabbedPane

private const val ACTION_TITLE = "Onlinesuche Tab ein-/ausblenden"
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
