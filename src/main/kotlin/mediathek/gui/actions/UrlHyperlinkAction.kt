package mediathek.gui.actions

import mediathek.config.MVConfig
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen
import mediathek.gui.messages.ProgramLocationChangedEvent
import mediathek.mac.escapeAppleScriptString
import mediathek.mainwindow.MediathekGui
import mediathek.tool.MessageBus
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.Desktop
import java.awt.event.ActionEvent
import java.io.IOException
import java.net.URI
import javax.swing.AbstractAction
import javax.swing.Action

class UrlHyperlinkAction(url: String) : AbstractAction(url) {
    init {
        putValue(Action.SHORT_DESCRIPTION, url)
    }

    override fun actionPerformed(event: ActionEvent) {
        openURL(event.actionCommand)
    }

    companion object {
        private val logger = LogManager.getLogger(UrlHyperlinkAction::class.java)
        private const val BROWSER_NOT_FOUND_TEXT =
            "\n Der Browser zum Anzeigen der URL wird nicht gefunden.\n Browser selbst auswählen."

        fun openURI(uri: URI) {
            openURL(uri.toString())
        }

        @JvmStatic
        fun openURL(url: String) {
            var launchFailed = false

            if (SystemUtils.IS_OS_MAC_OSX) {
                try {
                    launchMacDefaultBrowser(url)
                } catch (e: IOException) {
                    logger.error("Failed to launch default macOS web browser, using custom...", e)
                    launchFailed = true
                }
            } else if (SystemUtils.IS_OS_LINUX) {
                try {
                    launchApplication("xdg-open", url)
                } catch (e: IOException) {
                    logger.error("Failed to launch web browser with xdg-open", e)
                    launchFailed = true
                }
            } else if (Desktop.isDesktopSupported()) {
                try {
                    launchWithJavaDesktopServices(url)
                } catch (e: Exception) {
                    logger.error("Failed to launch java desktop supported web browser, using custom...", e)
                    launchFailed = true
                }
            } else {
                logger.trace("trying to launch custom web browser")
                configureAndStartCustomWebBrowser(url)
            }

            if (launchFailed) {
                configureAndStartCustomWebBrowser(url)
            }
        }

        private fun configureAndStartCustomWebBrowser(url: String) {
            try {
                val program = resolveBrowserProgram()
                launchApplication(program, url)

                MVConfig.add(MVConfig.Configs.SYSTEM_URL_OEFFNEN, program)
                MessageBus.messageBus.publishAsync(ProgramLocationChangedEvent())
            } catch (_: Exception) {
                MVConfig.add(MVConfig.Configs.SYSTEM_URL_OEFFNEN, "")
                logger.error("Failed to launch URL {} with custom browser", url)
            }
        }

        private fun resolveBrowserProgram(): String {
            val configuredProgram = MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN)
            if (configuredProgram.isNotEmpty()) {
                return configuredProgram
            }

            val dialog = DialogProgrammOrdnerOeffnen(MediathekGui.ui(), true, "", "Browser suchen", BROWSER_NOT_FOUND_TEXT)
            dialog.isVisible = true
            return if (dialog.ok) dialog.ziel else ""
        }

        private fun launchApplication(app: String, url: String) {
            logger.trace("trying to use xdg-open to start web browser")
            val builder = ProcessBuilder(app, url)
            builder.environment().remove("GDK_SCALE")
            builder.start()
        }

        private fun launchMacDefaultBrowser(url: String) {
            logger.trace("trying to launch macOS default web browser")
            ProcessBuilder("/usr/bin/osascript", "-e", "open location \"${escapeAppleScriptString(url)}\"").start()
        }

        private fun launchWithJavaDesktopServices(url: String) {
            logger.trace("trying to launch java desktop default web browser")
            val desktop = Desktop.getDesktop()
            if (desktop.isSupported(Desktop.Action.BROWSE)) {
                desktop.browse(URI(url))
            } else {
                throw UnsupportedOperationException("Desktop is not supported")
            }
        }
    }
}
