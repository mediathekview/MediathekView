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
package mediathek.gui.dialog.lucene_tutorial;

import mediathek.config.Konstanten;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.EscapeKeyHandler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import java.awt.*;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;

public final class LuceneTutorialDialog extends JDialog {
    private static final Logger logger = LogManager.getLogger();
    private static final String FALLBACK_HTML = """
            <html><body>
            <p>Die Lucene-Anleitung konnte nicht geladen werden.</p>
            </body></html>
            """;

    public LuceneTutorialDialog(Window owner) {
        super(owner, "Lucene-Suchsyntax", ModalityType.MODELESS);
        initComponents();
        loadTutorial();
        EscapeKeyHandler.installHandler(this, this::dispose);
    }

    private void loadTutorial() {
        try {
            var markdown = loadMarkdownResource();
            if (markdown != null) {
                logger.trace("Rendering Lucene tutorial from Markdown resource {}", Konstanten.PFAD_LUCENE_TUTORIAL_MARKDOWN);
                tutorialPane.setHtml(LuceneTutorialRenderer.renderMarkdown(markdown));
                return;
            }

            logger.error("Lucene tutorial Markdown resource could not be found");
            logger.trace("Showing inline fallback because no Lucene tutorial resource could be loaded");
            showFallbackHtml();
        } catch (Exception ex) {
            logger.error("Failed to load Lucene tutorial resource", ex);
            logger.trace("Showing inline fallback because loading the Lucene tutorial resource failed");
            showFallbackHtml();
        }
    }

    private String loadMarkdownResource() throws Exception {
        try (InputStream markdownStream = getClass().getResourceAsStream(Konstanten.PFAD_LUCENE_TUTORIAL_MARKDOWN)) {
            if (markdownStream == null) {
                return null;
            }
            return new String(markdownStream.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private void showFallbackHtml() {
        tutorialPane.setHtml(FALLBACK_HTML);
    }

    private void initComponents() {
        var scrollPane = new JScrollPane();
        tutorialPane = new FlexmarkHtmlPane();

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setType(Type.UTILITY);
        setMinimumSize(new Dimension(840, 700));

        tutorialPane.addHyperlinkListener(e -> {
            if (e.getEventType() != HyperlinkEvent.EventType.ACTIVATED || e.getURL() == null) {
                return;
            }

            var protocol = e.getURL().getProtocol();
            if ("http".equalsIgnoreCase(protocol) || "https".equalsIgnoreCase(protocol)) {
                try {
                    UrlHyperlinkAction.openURI(e.getURL().toURI());
                } catch (URISyntaxException ex) {
                    logger.error("Failed to open tutorial link {}", e.getURL(), ex);
                }
            }
        });
        scrollPane.setViewportView(tutorialPane);

        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout(8, 8));
        contentPane.add(scrollPane, BorderLayout.CENTER);

        pack();
        setLocationRelativeTo(getOwner());
    }

    private FlexmarkHtmlPane tutorialPane;
}
