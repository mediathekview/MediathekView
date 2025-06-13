/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.tabs.tab_film;

import com.formdev.flatlaf.util.ScaledImageIcon;
import mediathek.config.Konstanten;
import mediathek.daten.DatenFilm;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.CopyToClipboardAction;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXHyperlink;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.net.URI;
import java.util.Optional;
import java.util.function.Supplier;

public class FilmDescriptionPanel extends JPanel {
    private final JScrollPane scrollPane1 = new JScrollPane();
    private final JPopupMenu popupMenu = new JPopupMenu();
    private final SenderIconLabel lblIcon = new SenderIconLabel();
    private final JLabel lblThema = new JLabel();
    private final JLabel lblTitel = new JLabel();
    private final JTextArea textArea = new JTextArea();
    private final JXHyperlink hyperlink = new JXHyperlink();
    private DatenFilm currentFilm;

    public FilmDescriptionPanel() {

        initComponents();


        hyperlink.addActionListener(_ -> {
            if (!hyperlink.getToolTipText().isEmpty()) {
                var toolTipText = hyperlink.getToolTipText();
                if (Desktop.isDesktopSupported()) {
                    var d = Desktop.getDesktop();
                    if (d.isSupported(Desktop.Action.BROWSE)) {
                        try {
                            d.browse(new URI(toolTipText));
                        }
                        catch (Exception ex) {
                            SwingErrorDialog.showExceptionMessage(
                                    MediathekGui.ui(),
                                    "Es trat ein Fehler beim Öffnen des Links auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                                    ex);
                        }
                    }
                    else {
                        openUrl(toolTipText);
                    }
                }
                else {
                    openUrl(toolTipText);
                }
            }
        });

        createPopupMenu();

        setAllFieldsEmpty();
    }

    private void createPopupMenu() {
        var item = new JMenuItem("Beschreibung ändern...");
        item.addActionListener(_ -> {
            DialogFilmBeschreibung dialog = new DialogFilmBeschreibung(MediathekGui.ui(), currentFilm);
            dialog.setVisible(true);
        });
        popupMenu.add(item);
        popupMenu.addSeparator();

        item = new JMenuItem("Beschreibung in Zwischenablage kopieren");
        item.addActionListener(_ -> GuiFunktionen.copyToClipboard(currentFilm.getDescription()));
        popupMenu.add(item);

        item = new JMenuItem("Filmbasisinformationen in Zwischenablage kopieren");
        item.addActionListener(_ -> {
            String sb = currentFilm.getSender() +
                    " - " +
                    currentFilm.getThema() +
                    " - " +
                    currentFilm.getTitle();

            GuiFunktionen.copyToClipboard(sb);
        });
        popupMenu.add(item);

        popupMenu.addSeparator();
        item = new JMenuItem("Auswahl kopieren");
        item.addActionListener(_ -> {
            final var selected = (textArea.getSelectionEnd() - textArea.getSelectionStart()) > 0;
            if (!selected) {
                JOptionPane.showMessageDialog(this, "Kein Text markiert!", Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
            }
            else {
                var text = textArea.getSelectedText();
                GuiFunktionen.copyToClipboard(text);
            }
        });
        popupMenu.add(item);

        setComponentPopupMenu(popupMenu);
        textArea.setComponentPopupMenu(popupMenu);
    }

    private void initComponents() {
        setLayout(new MigLayout(
                new LC().hideMode(3),
                new AC()
                        .fill().gap()
                        .grow().fill(),
                new AC()
                        .gap()
                        .gap()
                        .gap()
        ));

        lblIcon.setPreferredSize(new Dimension(96, 96));
        lblIcon.setVerticalAlignment(SwingConstants.TOP);
        add(lblIcon, new CC().cell(0, 0, 1, 3).alignX("center").alignY("top").grow(0, 0)); //NON-NLS

        lblThema.setFont(lblThema.getFont().deriveFont(lblThema.getFont().getStyle() | Font.BOLD));
        add(lblThema, new CC().cell(1, 0));

        lblTitel.setFont(lblTitel.getFont().deriveFont(lblTitel.getFont().getStyle() | Font.BOLD));
        add(lblTitel, new CC().cell(1, 1));

        scrollPane1.setPreferredSize(new Dimension(299, 75));
        scrollPane1.setMaximumSize(new Dimension(Integer.MAX_VALUE, 75));
        scrollPane1.setMinimumSize(new Dimension(23, 75));
        scrollPane1.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        textArea.setEditable(false);
        textArea.setWrapStyleWord(true);
        textArea.setLineWrap(true);
        scrollPane1.setViewportView(textArea);
        add(scrollPane1, new CC().cell(1, 2).grow());

        //add(hyperlink, new CC().cell(0, 3, 2, 1));
        add(hyperlink, new CC().cell(1, 3));
    }

    public void setCurrentFilm(@Nullable DatenFilm film) {
        if (film == null) {
            setAllFieldsEmpty();
        }
        else {
            showFilmDescription(film);
        }
        currentFilm = film;
    }

    public void install(@NotNull JTabbedPane tabbedPane, @NotNull JTable tabelle, @NotNull Supplier<Optional<DatenFilm>> filmSupplier) {
        tabbedPane.add("Beschreibung", this);
        tabelle.getSelectionModel().addListSelectionListener(_ ->
                filmSupplier.get().ifPresentOrElse(this::setCurrentFilm, () -> setCurrentFilm(null)));
    }

    private void setAllFieldsEmpty() {
        hyperlink.setVisible(false);
        hyperlink.setText("");
        hyperlink.setToolTipText("");
        hyperlink.setComponentPopupMenu(null);

        textArea.setText("");
        lblIcon.setIcon(null);
        lblThema.setText("");
        lblTitel.setText("");
    }

    private void openUrl(String url) {
            UrlHyperlinkAction.openURL(url);
    }

    private void showFilmDescription(@NotNull DatenFilm film) {
        lblThema.setText(film.getThema());
        lblTitel.setText(film.getTitle());

        hyperlink.setVisible(true);
        try {
            hyperlink.setText("Link zur Webseite");
            hyperlink.setClicked(false);
            JPopupMenu popup = new JPopupMenu();
            popup.add(new CopyToClipboardAction(film.getWebsiteUrl()));
            hyperlink.setComponentPopupMenu(popup);
        }
        catch (Exception e) {
            hyperlink.setText("Link nicht verfügbar");
        }
        hyperlink.setToolTipText(film.getWebsiteUrl());

        textArea.setText(film.getDescription());
        SwingUtilities.invokeLater(() -> scrollPane1.getVerticalScrollBar().setValue(0));
        lblIcon.setSender(film.getSender());
    }

    static class SenderIconLabel extends JLabel {
        private static final Dimension ICON_DIMENSION = new Dimension(96, 96);

        public SenderIconLabel() {
            setText("");
            setIcon(null);
        }

        private void sizeToIcon(@NotNull Icon icon) {
            int height = icon.getIconHeight();
            int width = icon.getIconWidth();

            Dimension d = new Dimension(width, height);
            setPreferredSize(d);
        }

        public void setSender(@Nullable String sender) {
            if (sender == null) {
                setIcon(null);
            }
            else {
                MVSenderIconCache.get(sender).ifPresentOrElse(icon -> {
                    var imageDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                    var destDim = GuiFunktionen.calculateFittedDimension(imageDim, ICON_DIMENSION);
                    var origIcon = new ScaledImageIcon(icon, destDim.width, destDim.height);
                    setText("");
                    setIcon(origIcon);
                    sizeToIcon(origIcon);
                }, () -> {
                    setIcon(null);
                    setText(sender);
                });
            }
        }
    }
}
