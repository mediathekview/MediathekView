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
package mediathek.gui.dialog.lucene_tutorial

import mediathek.config.Konstanten
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.tool.EscapeKeyHandler
import org.apache.logging.log4j.LogManager
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Window
import java.net.URISyntaxException
import javax.swing.JDialog
import javax.swing.JScrollPane
import javax.swing.WindowConstants
import javax.swing.event.HyperlinkEvent

class LuceneTutorialDialog(
    owner: Window,
    title: String = "Lucene-Suchsyntax",
    private val markdownResourcePath: String = Konstanten.PFAD_LUCENE_TUTORIAL_MARKDOWN,
) : JDialog(owner, title, ModalityType.MODELESS) {
    private val tutorialPane = FlexmarkHtmlPane()

    init {
        initComponents()
        loadTutorial()
        EscapeKeyHandler.installHandler(this) { dispose() }
    }

    private fun loadTutorial() {
        try {
            val markdown = loadMarkdownResource()
            if (markdown != null) {
                logger.trace("Rendering Markdown help from resource {}", markdownResourcePath)
                tutorialPane.setHtml(LuceneTutorialRenderer.renderMarkdown(markdown))
                return
            }

            logger.error("Markdown help resource could not be found: {}", markdownResourcePath)
            logger.trace("Showing inline fallback because no Markdown help resource could be loaded")
            showFallbackHtml()
        } catch (ex: Exception) {
            logger.error("Failed to load Markdown help resource {}", markdownResourcePath, ex)
            logger.trace("Showing inline fallback because loading the Markdown help resource failed")
            showFallbackHtml()
        }
    }

    private fun loadMarkdownResource(): String? =
        javaClass.getResourceAsStream(markdownResourcePath)?.bufferedReader(Charsets.UTF_8)?.use { it.readText() }

    private fun showFallbackHtml() {
        tutorialPane.setHtml(FALLBACK_HTML)
    }

    private fun initComponents() {
        defaultCloseOperation = WindowConstants.DISPOSE_ON_CLOSE
        type = Window.Type.UTILITY
        minimumSize = Dimension(840, 700)

        tutorialPane.addHyperlinkListener { event ->
            if (event.eventType != HyperlinkEvent.EventType.ACTIVATED || event.url == null) {
                return@addHyperlinkListener
            }

            val protocol = event.url.protocol
            if (protocol.equals("http", ignoreCase = true) || protocol.equals("https", ignoreCase = true)) {
                try {
                    UrlHyperlinkAction.openURI(event.url.toURI())
                } catch (ex: URISyntaxException) {
                    logger.error("Failed to open tutorial link {}", event.url, ex)
                }
            }
        }

        contentPane.layout = BorderLayout(8, 8)
        contentPane.add(JScrollPane(tutorialPane), BorderLayout.CENTER)

        pack()
        setLocationRelativeTo(owner)
    }

    private companion object {
        private val logger = LogManager.getLogger()
        private val FALLBACK_HTML = """
            <html><body>
            <p>Die Hilfe konnte nicht geladen werden.</p>
            </body></html>
            """.trimIndent()
    }
}
