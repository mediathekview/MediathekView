package mediathek.gui.tabs.tab_online_search

import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.tool.GuiFunktionen
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
        add(JMenuItem("Website öffnen").apply {
            isEnabled = selectedResult?.websiteUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { UrlHyperlinkAction.openURL(selectedResult.websiteUrl) }
        })
        add(JMenuItem("Website kopieren").apply {
            isEnabled = selectedResult?.websiteUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.websiteUrl) }
        })
        add(JMenuItem("URL kopieren").apply {
            isEnabled = selectedResult?.normalQualityUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.normalQualityUrl) }
        })
        add(JMenuItem("HD-URL kopieren").apply {
            isEnabled = selectedResult?.highQualityUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.highQualityUrl) }
        })
        add(JMenuItem("Kleine URL kopieren").apply {
            isEnabled = selectedResult?.lowQualityUrl?.isNotEmpty() == true
            if (selectedResult != null) addActionListener { GuiFunktionen.copyToClipboard(selectedResult.lowQualityUrl) }
        })
    }
}
