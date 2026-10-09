/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialog

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.BuildInfo
import mediathek.config.Konstanten
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.tool.EscapeKeyHandler
import org.apache.commons.lang3.SystemUtils
import org.jdesktop.swingx.JXHyperlink
import java.awt.Window
import javax.swing.SwingUtilities

class AboutDialog(owner: Window?) : AboutDialogBase(owner), CoroutineScope {
    private val job = SupervisorJob()
    override val coroutineContext = job + Dispatchers.Swing

    init {
        lblVersion.text = versionText()
        installCloseHandler()
        installHyperlinkActions()
        resetFormerContributorsScrollPosition()
        loadBuildInfo()
    }

    override fun dispose() {
        job.cancel()
        super.dispose()
    }

    private fun installCloseHandler() {
        EscapeKeyHandler.installHandler(this, ::dispose)
    }

    private fun installHyperlinkActions() {
        listOf(
            hyperlinkHomepage to Konstanten.ADRESSE_WEBSITE,
            hyperlinkGuiDonation to "https://paypal.me/ChristianFranzke",
            hyperlinkServerDonation to Konstanten.ADRESSE_DONATION,
            hyperlinkForum to Konstanten.ADRESSE_FORUM,
            hyperlinkOnlineHelp to Konstanten.ADRESSE_ONLINE_HELP,
            hyperlinkFaq to Konstanten.ADRESSE_ONLINE_FAQ,
            hyperlinkJetBrains to "https://www.jetbrains.com",
            hyperlinkEjTechnologies to "https://www.ej-technologies.com",
        ).forEach { (hyperlink, url) -> hyperlink.openUrlOnClick(url) }
    }

    private fun JXHyperlink.openUrlOnClick(url: String) {
        addActionListener { UrlHyperlinkAction.openURL(url) }
    }

    private fun resetFormerContributorsScrollPosition() {
        SwingUtilities.invokeLater {
            scrollPane1.verticalScrollBar.value = 0
        }
    }

    private fun loadBuildInfo() {
        launch {
            val buildInfo = withContext(Dispatchers.IO) { BuildInfo.current() }
            lblVersion.text = versionText(buildInfo)
        }
    }

    private fun versionText(buildInfo: BuildInfo? = null): String =
        if (buildInfo?.hasGitMetadata() == true) {
            "<html>${baseVersionText()}<br/>Build: ${buildInfo.formatForDisplay()}<br/>JDK: ${runtimeJdkText()}</html>"
        } else {
            "<html>${baseVersionText()}<br/>JDK: ${runtimeJdkText()}</html>"
        }

    private fun baseVersionText(): String =
        "Version ${Konstanten.MVVERSION} (${SystemUtils.OS_ARCH})"

    private fun runtimeJdkText(): String =
        "${System.getProperty("java.version")} (${System.getProperty("java.vendor")})"
}
