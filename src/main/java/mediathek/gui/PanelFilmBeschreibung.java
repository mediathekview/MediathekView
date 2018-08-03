/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.daten.DatenFilm;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVFont;
import mediathek.tool.table.MVDownloadsTable;
import mediathek.tool.table.MVFilmTable;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableModel;
import java.awt.*;
import java.net.URISyntaxException;

@SuppressWarnings("serial")
public class PanelFilmBeschreibung extends JPanel implements ListSelectionListener {
    private DatenFilm currentFilm = null;
    private final MVTable table;

    private JMenuItem createCopyLinkToClipboardItem() {
        JMenuItem item = new JMenuItem("URL kopieren");
        item.addActionListener(e -> {
            if (currentFilm != null)
                GuiFunktionen.copyToClipboard(currentFilm.getWebsiteLink());
        });

        return item;
    }

    public PanelFilmBeschreibung(Daten daten, MVTable table, boolean film) {
        initComponents();
        this.table = table;

        Icon closeIcon = IconFontSwing.buildIcon(FontAwesome.TIMES_CIRCLE_O, 16);
        jCheckBoxBeschreibung.setIcon(closeIcon);
        jCheckBoxBeschreibung.addActionListener(e -> {
            if (film) {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN, Boolean.FALSE.toString());
                Listener.notify(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, PanelFilmBeschreibung.class.getSimpleName());
            } else {
                MVConfig.add(MVConfig.Configs.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN, Boolean.FALSE.toString());
                Listener.notify(Listener.EREIGNIS_DOWNLOAD_BESCHREIBUNG_ANZEIGEN, PanelFilmBeschreibung.class.getSimpleName());
            }
        });

        hyperlinkButton.addActionListener(e -> {
            try {
                if (currentFilm != null)
                    UrlHyperlinkAction.openURL(null, currentFilm.getWebsiteLink());
            } catch (URISyntaxException e1) {
                logger.error(e1);
            }
        });

        JPopupMenu popupMenu = new JPopupMenu();
        popupMenu.add(createCopyLinkToClipboardItem());
        hyperlinkButton.setComponentPopupMenu(popupMenu);

        Icon editIcon = IconFontSwing.buildIcon(FontAwesome.PENCIL_SQUARE_O, 16);
        jCheckBoxChange.setIcon(editIcon);
        jCheckBoxChange.addActionListener(e -> {
            if (currentFilm != null) {
                final String oldDescription = currentFilm.getDescription();
                new DialogFilmBeschreibung(daten.getMediathekGui(), daten, currentFilm).setVisible(true);
                if (!currentFilm.getDescription().equals(oldDescription)) {
                    // dann hat sich die Beschreibung geändert
                    setText();
                    daten.filmlisteSpeichern();
                    Listener.notify(Listener.EREIGNIS_BESCHREIBUNG, PanelFilmBeschreibung.class.getSimpleName());
                }
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_FONT, PanelFilmBeschreibung.class.getSimpleName()) {
            @Override
            public void ping() {
                setText();
            }
        });

        table.getSelectionModel().addListSelectionListener(this);

        //update for first time...
        updateFilmData();
    }

    private void updateFilmData() {
        final int selectedTableRow = table.getSelectedRow();
        if (selectedTableRow >= 0) {
            DatenFilm film;
            final TableModel model = table.getModel();
            final int modelIndex = table.convertRowIndexToModel(selectedTableRow);

            if (table instanceof MVFilmTable) {
                film = (DatenFilm) model.getValueAt(modelIndex, DatenFilm.FILM_REF);
            } else if (table instanceof MVDownloadsTable) {
                film = ((DatenDownload) model.getValueAt(modelIndex, DatenDownload.DOWNLOAD_REF)).film;
            } else {
                logger.debug("UNHANDLED TABLE TYPE!!!");
                film = null;
            }

            displayFilmData(film);
        } else {
            displayFilmData(null);
        }
    }

    private static final Logger logger = LogManager.getLogger(PanelFilmBeschreibung.class);

    private void displayFilmData(DatenFilm aaktFilm) {
        currentFilm = aaktFilm;
        setText();
    }

    private void setText() {
        if (currentFilm == null) {
            jEditorPane.setText("");
            hyperlinkButton.setToolTipText("");
        } else {
            // Beschreibung setzen
            jEditorPane.setText(
                    "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
                    + "<head><style type=\"text/css\">.sans { font-family: Verdana, Geneva, sans-serif; font-size: " + MVFont.fontSize + "pt; }</style></head>\n"
                    + "<body>"
                            + "<span class=\"sans\"><b>" + (currentFilm.getSender().isEmpty() ? "" : currentFilm.getSender() + "  -  ")
                            + currentFilm.getTitle() + "</b><br /></span>"
                            + "<span class=\"sans\">" + currentFilm.getDescription() + "</span>"
                    + "</body>"
                    + "</html>");
            jEditorPane.setCaretPosition(0);

            hyperlinkButton.setToolTipText(currentFilm.getWebsiteLink());
        }
    }

    @Override
    public void valueChanged(ListSelectionEvent e) {
        if (!e.getValueIsAdjusting()) {
            updateFilmData();
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jCheckBoxBeschreibung = new JCheckBox();
        JPanel jPanel1 = new JPanel();
        JScrollPane jScrollPane2 = new JScrollPane();
        jEditorPane = new JEditorPane();
        jCheckBoxChange = new JCheckBox();
        hyperlinkButton = new HyperlinkButton();

        //======== this ========

        //---- jCheckBoxBeschreibung ----
        jCheckBoxBeschreibung.setToolTipText("Beschreibung ausblenden");

        //======== jPanel1 ========
        {

            //======== jScrollPane2 ========
            {
                jScrollPane2.setBorder(new LineBorder(new Color(153, 153, 153)));

                //---- jEditorPane ----
                jEditorPane.setEditable(false);
                jEditorPane.setBorder(new EmptyBorder(4, 4, 4, 4));
                jEditorPane.setContentType("text/html");
                jEditorPane.setFont(new Font(Font.DIALOG, Font.PLAIN, 24));
                jScrollPane2.setViewportView(jEditorPane);
            }

            //---- jCheckBoxChange ----
            jCheckBoxChange.setToolTipText("Beschreibung \u00e4ndern");
            jCheckBoxChange.setIcon(null);

            //---- hyperlinkButton ----
            hyperlinkButton.setText("Link zur Webseite");

            GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
            jPanel1.setLayout(jPanel1Layout);
            jPanel1Layout.setHorizontalGroup(
                    jPanel1Layout.createParallelGroup()
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                    .addContainerGap()
                                    .addComponent(hyperlinkButton, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addGap(218, 218, 218)
                                    .addComponent(jCheckBoxChange)
                                    .addContainerGap())
                            .addComponent(jScrollPane2)
            );
            jPanel1Layout.setVerticalGroup(
                    jPanel1Layout.createParallelGroup()
                            .addGroup(GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                    .addComponent(jScrollPane2, GroupLayout.PREFERRED_SIZE, 84, GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(jPanel1Layout.createParallelGroup()
                                            .addComponent(jCheckBoxChange)
                                            .addComponent(hyperlinkButton, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                                    .addContainerGap())
            );
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup()
                        .addGroup(layout.createSequentialGroup()
                                .addComponent(jCheckBoxBeschreibung)
                                .addGap(5, 5, 5)
                                .addComponent(jPanel1, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGap(5, 5, 5))
        );
        layout.setVerticalGroup(
                layout.createParallelGroup()
                        .addComponent(jCheckBoxBeschreibung)
                        .addGroup(layout.createSequentialGroup()
                                .addGap(5, 5, 5)
                                .addComponent(jPanel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox jCheckBoxBeschreibung;
    private JEditorPane jEditorPane;
    private JCheckBox jCheckBoxChange;
    private HyperlinkButton hyperlinkButton;
    // End of variables declaration//GEN-END:variables
}
