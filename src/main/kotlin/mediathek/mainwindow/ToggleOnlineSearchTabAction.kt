package mediathek.mainwindow

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.tabs.tab_online_search.OnlineSearchPanel
import javax.swing.JTabbedPane

private const val ACTION_TITLE = "Onlinesuche Tab ein-/ausblenden"
private const val TAB_TITLE = "Onlinesuche"
private const val PREFERRED_INSERT_INDEX = 2

class ToggleOnlineSearchTabAction(
    tabbedPane: JTabbedPane,
    onlineSearchPanel: OnlineSearchPanel,
) : ToggleOptionalTabAction(
    tabbedPane,
    onlineSearchPanel,
    ACTION_TITLE,
    TAB_TITLE,
    { ApplicationConfiguration.getInstance().onlineSearchTabVisible = it },
    PREFERRED_INSERT_INDEX,
)
