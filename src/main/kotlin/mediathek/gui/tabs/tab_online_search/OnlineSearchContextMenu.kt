package mediathek.gui.tabs.tab_online_search

import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.tool.GuiFunktionen
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

class OnlineSearchContextMenu(
    selectedResult: OnlineSearchResult?,
    selectedResults: List<OnlineSearchResult>,
    host: OnlineSearchHost,
) : JPopupMenu() {
    init {
        add(JMenuItem("Abspielen").apply {
            isEnabled = selectedResult != null
            if (selectedResult != null) addActionListener { host.playResult(selectedResult) }
        })
        add(JMenuItem("Filminformation anzeigen").apply {
            isEnabled = selectedResult != null
            if (selectedResult != null) addActionListener { host.showFilmInfo(selectedResult) }
        })
        add(JMenuItem("Download anlegen").apply {
            isEnabled = selectedResult != null
            if (selectedResult != null) {
                addActionListener { host.startDownload(selectedResults.ifEmpty { listOf(selectedResult) }) }
            }
        })
        addSeparator()
        add(JMenuItem("Webseite im Browser öffnen").apply {
            isEnabled = selectedResult?.websiteUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { UrlHyperlinkAction.openURL(selectedResult.websiteUrl) }
        })
        add(JMenuItem("Website-URL kopieren").apply {
            isEnabled = selectedResult?.websiteUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.websiteUrl) }
        })
        add(JMenu("Film-URL kopieren").apply {
            add(JMenuItem("höchste/hohe Qualität").apply {
                isEnabled = selectedResult?.highQualityUrl?.isNotEmpty() == true
                if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.highQualityUrl) }
            })
            add(JMenuItem("mittlere Qualität").apply {
                isEnabled = selectedResult?.normalQualityUrl?.isNotEmpty() == true
                if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.normalQualityUrl) }
            })
            add(JMenuItem("niedrige Qualität").apply {
                isEnabled = selectedResult?.lowQualityUrl?.isNotEmpty() == true
                if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.lowQualityUrl) }
            })
            isEnabled = menuComponents.filterIsInstance<JMenuItem>().any { it.isEnabled }
        })
    }
}
