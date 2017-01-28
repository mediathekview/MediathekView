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

import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.tool.Filter;
import org.jdesktop.swingx.JXSearchField;

import javax.swing.*;
import javax.swing.Box.Filler;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

@SuppressWarnings("serial")
public final class ToolBar extends JToolBar {
    private static final Dimension MINIMUM_DIMENSION = new Dimension(1, 5);
    private static final Dimension BUTTON_DIMENSION = new Dimension(32, 32);
    private static final Filler HORIZONTAL_FILLER = new Filler(MINIMUM_DIMENSION, MINIMUM_DIMENSION, new Dimension(Integer.MAX_VALUE, 5));
    private static final String FILM_ABSPIELEN_TEXT = "Film abspielen";
    private static final String FILM_RECORD_TEXT = "Film aufzeichnen";
    private static final String SHOW_FILMINFO_TEXT = "Filminformation anzeigen";
    private static final String FILTER_TEXT = "Filter anzeigen/ausblenden";
    private static final String UPDATE_DOWNLOADS_TEXT = "Liste der Downloads aktualisieren";
    private static final String START_ALL_DOWNLOADS_TEXT = "alle Downloads starten";
    private static final String REMOVE_DOWNLOADS_TEXT = "Downloads aus Liste entfernen";
    private static final String CLEANUP_DL_LIST_TEXT = "Liste der Downloads aufräumen";
    private static final String PUTBACK_DL_TEXT = "Downloads zurückstellen";
    private static final String ABO_ON_TEXT = "Abos einschalten";
    private static final String ABO_OFF_TEXT = "Abos ausschalten";
    private static final String ABO_DELETE_TEXT = "Abos löschen";
    private static final String ABO_CHANGE_TEXT = "Abos ändern";
    private final Daten daten;
    private final BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    private final MediathekGui.TABS state;
    private final ArrayList<MVToolBarButton> buttonList = new ArrayList<>();
    public JXSearchField jTextFieldFilter;
    private MVToolBarButton jButtonAboAendern = null;
    private MVToolBarButton jButtonAbosAusschalten = null;
    private MVToolBarButton jButtonAbosEinschalten = null;
    private MVToolBarButton jButtonAbosLoeschen = null;
    private MVToolBarButton jButtonDownloadAktualisieren = null;
    private MVToolBarButton jButtonDownloadAlleStarten = null;
    private MVToolBarButton jButtonDownloadAufraeumen = null;
    private MVToolBarButton jButtonDownloadFilmStarten = null;
    private MVToolBarButton jButtonDownloadLoeschen = null;
    private MVToolBarButton jButtonDownloadZurueckstellen = null;
    private MVToolBarButton jButtonFilmAbspielen = null;
    private MVToolBarButton jButtonFilmSpeichern = null;
    private JButton jButtonFilterPanel = null;
    private MVToolBarButton jButtonInfo = null;
    private MVConfig.Configs nrToolbar = null;
    private MVConfig.Configs nrIconKlein = MVConfig.Configs.SYSTEM_ICON_KLEIN;
    private MVToolBarButton jButtonFilmlisteLaden = null;

    public ToolBar(Daten ddaten, MediathekGui.TABS state) {
        super();
        daten = ddaten;
        this.state = state;

        createFilmlisteLadenButton();
        createButtonInfo();

        switch (state) {
            case FILME:
                nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_FILME;
                break;
            case DOWNLOADS:
                nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_DOWNLOAD;
                break;
            case ABOS:
                nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_ABO;
                break;
            default:
                nrToolbar = null;
                nrIconKlein = null;
        }
        startup();
        setToolbar();
        Listener.addListener(new Listener(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, ToolBar.class.getSimpleName() + state) {
            @Override
            public void ping() {
                setToolBarIconSize(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
            }
        });
    }

    private void startup() {
        setBorder(BorderFactory.createLineBorder(new Color(153, 153, 153)));
        setFloatable(false);

        switch (state) {
            case FILME:
                startupFilme();
                break;
            case DOWNLOADS:
                startupDownload();
                break;
            case ABOS:
                startupAbo();
                break;
        }

        // Icons
        setToolBarIconSize(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        readToolBarButtonStates();
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getSimpleName()) {
            @Override
            public void ping() {
                filterAnzeigen();
            }
        });
        addMouseListener(beobMausToolBar);
    }

    private void setupSearchField() {
        jTextFieldFilter = new JXSearchField();
        jTextFieldFilter.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTextFieldFilter.setToolTipText("Thema/Titel suchen");
        jTextFieldFilter.setDisabledTextColor(new Color(102, 102, 102));
        jTextFieldFilter.setMaximumSize(new Dimension(300, 35));
        jTextFieldFilter.setName("Thema/Titel");
        jTextFieldFilter.setPreferredSize(new Dimension(300, 25));
        jTextFieldFilter.setPrompt("Thema/Titel");
        jTextFieldFilter.setLayoutStyle(JXSearchField.LayoutStyle.MAC);
        jTextFieldFilter.setSearchMode(JXSearchField.SearchMode.INSTANT);
        jTextFieldFilter.setUseNativeSearchFieldIfPossible(true);
        jTextFieldFilter.getFindButton().setIcon(Icons.ICON_SUCHEN);
        jTextFieldFilter.addActionListener(actionEvent -> {
            Filter.checkPattern2(jTextFieldFilter);
            Daten.guiFilme.guiFilmeFiltern();
        });

        jTextFieldFilter.setInstantSearchDelay(150);
        this.add(jTextFieldFilter);
    }

    private void createButtonInfo() {
        jButtonInfo = new MVToolBarButton(SHOW_FILMINFO_TEXT, SHOW_FILMINFO_TEXT, Icons.ICON_TOOLBAR_FILME_INFO_GR, Icons.ICON_TOOLBAR_FILME_INFO_KL);
        jButtonInfo.addActionListener(e -> daten.getMediathekGui().getFilmInformationHud().showInfo());
    }

    private void startupFilme() {
        add(jButtonFilmlisteLaden);
        addSeparator();
        add(jButtonInfo);
        addSeparator();
        jButtonFilmAbspielen = new MVToolBarButton(FILM_ABSPIELEN_TEXT, FILM_ABSPIELEN_TEXT, Icons.ICON_TOOLBAR_FILME_FILM_START_GR, Icons.ICON_TOOLBAR_FILME_FILM_START_KL);
        jButtonFilmAbspielen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmAbspielen());
        add(jButtonFilmAbspielen);
        jButtonFilmSpeichern = new MVToolBarButton(FILM_RECORD_TEXT, FILM_RECORD_TEXT, Icons.ICON_TOOLBAR_FILME_REC_GR, Icons.ICON_TOOLBAR_FILME_REC_KL);
        jButtonFilmSpeichern.addActionListener(e -> Daten.guiFilme.guiFilmeFilmSpeichern());
        add(jButtonFilmSpeichern);
        add(HORIZONTAL_FILLER);

        jButtonFilterPanel = new JButton();
        setupSearchField();
        setupFilterButton("Erweiterte Suche (Filter) anzeigen/ausblenden");
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER));
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.toString(b));
            filterAnzeigen();
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getName());
        });
    }

    private void startupDownload() {
        add(jButtonFilmlisteLaden);
        addSeparator();
        add(jButtonInfo);
        addSeparator();
        jButtonDownloadAktualisieren = new MVToolBarButton(UPDATE_DOWNLOADS_TEXT, UPDATE_DOWNLOADS_TEXT, Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_GR, Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_KL);
        add(jButtonDownloadAktualisieren);
        jButtonDownloadAlleStarten = new MVToolBarButton(START_ALL_DOWNLOADS_TEXT, START_ALL_DOWNLOADS_TEXT, Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_GR, Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_KL);
        jButtonDownloadAlleStarten.addActionListener(e -> Daten.guiDownloads.starten(true));
        add(jButtonDownloadAlleStarten);
        addSeparator();
        jButtonDownloadFilmStarten = new MVToolBarButton("Film Starten", "gespeicherten Film abspielen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_KL);
        jButtonDownloadFilmStarten.addActionListener(e -> Daten.guiDownloads.filmAbspielen());
        add(jButtonDownloadFilmStarten);
        addSeparator();
        jButtonDownloadZurueckstellen = new MVToolBarButton(PUTBACK_DL_TEXT, PUTBACK_DL_TEXT, Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_KL);
        jButtonDownloadZurueckstellen.addActionListener(e -> Daten.guiDownloads.zurueckstellen());
        add(jButtonDownloadZurueckstellen);
        jButtonDownloadLoeschen = new MVToolBarButton(REMOVE_DOWNLOADS_TEXT, REMOVE_DOWNLOADS_TEXT, Icons.ICON_TOOLBAR_DOWNLOAD_DEL_GR, Icons.ICON_TOOLBAR_DOWNLOAD_DEL_KL);
        jButtonDownloadLoeschen.addActionListener(e -> Daten.guiDownloads.loeschen());
        add(jButtonDownloadLoeschen);
        jButtonDownloadAufraeumen = new MVToolBarButton(CLEANUP_DL_LIST_TEXT, CLEANUP_DL_LIST_TEXT, Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_GR, Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_KL);
        jButtonDownloadAufraeumen.addActionListener(e -> Daten.guiDownloads.aufraeumen());
        add(jButtonDownloadAufraeumen);
        jButtonDownloadAktualisieren.addActionListener(e -> Daten.guiDownloads.aktualisieren());
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                jButtonDownloadAktualisieren.setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                jButtonDownloadAktualisieren.setEnabled(true);
            }
        });

        addSeparator();
        add(HORIZONTAL_FILLER);

        setupFilterButton(FILTER_TEXT);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_DOWNLOAD_FILTER_ANZEIGEN, ToolBar.class.getName());
        });
    }

    private void setupFilterButton(String tooltipText) {
        jButtonFilterPanel = new JButton();
        jButtonFilterPanel.setToolTipText(tooltipText);
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setBorderPainted(false);
        jButtonFilterPanel.setHorizontalTextPosition(SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(BUTTON_DIMENSION);
        jButtonFilterPanel.setMinimumSize(BUTTON_DIMENSION);
        jButtonFilterPanel.setPreferredSize(BUTTON_DIMENSION);
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setVerticalTextPosition(SwingConstants.BOTTOM);
        jButtonFilterPanel.setIcon(Icons.ICON_BUTTON_FILTER_ANZEIGEN);
        add(jButtonFilterPanel);
    }

    private void startupAbo() {
        addSeparator();
        jButtonAbosEinschalten = new MVToolBarButton(ABO_ON_TEXT, ABO_ON_TEXT, Icons.ICON_TOOLBAR_ABO_EIN_GR, Icons.ICON_TOOLBAR_ABO_EIN_KL);
        jButtonAbosEinschalten.addActionListener(e -> Daten.guiAbo.einAus(true));
        add(jButtonAbosEinschalten);
        jButtonAbosAusschalten = new MVToolBarButton(ABO_OFF_TEXT, ABO_OFF_TEXT, Icons.ICON_TOOLBAR_ABO_AUS_GR, Icons.ICON_TOOLBAR_ABO_AUS_KL);
        jButtonAbosAusschalten.addActionListener(e -> Daten.guiAbo.einAus(false));
        add(jButtonAbosAusschalten);
        jButtonAbosLoeschen = new MVToolBarButton(ABO_DELETE_TEXT, ABO_DELETE_TEXT, Icons.ICON_TOOLBAR_ABO_DEL_GR, Icons.ICON_TOOLBAR_ABO_DEL_KL);
        jButtonAbosLoeschen.addActionListener(e -> Daten.guiAbo.loeschen());
        add(jButtonAbosLoeschen);
        jButtonAboAendern = new MVToolBarButton(ABO_CHANGE_TEXT, ABO_CHANGE_TEXT, Icons.ICON_TOOLBAR_ABO_CONFIG_GR, Icons.ICON_TOOLBAR_ABO_CONFIG_KL);
        jButtonAboAendern.addActionListener(e -> Daten.guiAbo.aendern());
        add(jButtonAboAendern);

        addSeparator();
        add(HORIZONTAL_FILLER);

        setupFilterButton(FILTER_TEXT);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, ToolBar.class.getName());
        });
    }

    private void createFilmlisteLadenButton() {
        jButtonFilmlisteLaden = new MVToolBarButton("", "", Icons.ICON_TOOLBAR_FILME_FILMLISTE_LADEN_GR, Icons.ICON_TOOLBAR_FILME_FILMLISTE_LADEN_KL);
        add(jButtonFilmlisteLaden);
        jButtonFilmlisteLaden.setAction(daten.getMediathekGui().loadFilmlistAction);
    }

    private void setToolBarIconSize(boolean klein) {
        MVConfig.add(nrIconKlein, Boolean.toString(klein));
        beobMausToolBar.itemKlein.setSelected(klein);
        for (MVToolBarButton b : buttonList) {
            b.setIconSize();
        }
        repaint();
    }

    private void setToolbar() {
        filterAnzeigen();
        for (MVToolBarButton b : buttonList) {
            b.setVisible(b.anzeigen);
        }
    }

    private void filterAnzeigen() {
        if (state == MediathekGui.TABS.FILME) {
            jTextFieldFilter.setVisible(!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
        }
    }

    /**
     * Read and set the Toolbar button states from config file.
     */
    private void readToolBarButtonStates() {
        if (nrToolbar != null) {
            String[] b = MVConfig.get(nrToolbar).split(":");
            if (buttonList.size() == b.length) {
                // ansonsten gibt es neue Button: dann alle anzeigen
                for (int i = 0; i < b.length; ++i) {
                    buttonList.get(i).anzeigen = Boolean.parseBoolean(b[i]);
                    buttonList.get(i).setVisible(Boolean.parseBoolean(b[i]));
                }
            }
        }
        setToolbar();
        if (nrIconKlein != null) {
            setToolBarIconSize(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        }
    }

    /**
     * Write the ToolBar button states to config file.
     */
    private void writeToolBarButtonStates() {
        if (nrToolbar != null) {
            MVConfig.add(nrToolbar, "");
            for (MVToolBarButton b : buttonList) {
                if (!MVConfig.get(nrToolbar).isEmpty()) {
                    MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + ':');
                }
                MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + Boolean.toString(b.anzeigen));
            }
        }
    }

    private class MVToolBarButton extends JButton {
        private final ImageIcon imageIconKlein;
        private final ImageIcon imageIconNormal;
        private boolean anzeigen = true;
        private String name = "";

        public MVToolBarButton(String name, String ttoolTip,
                               ImageIcon iimageIconNormal, ImageIcon iimageIconKlein) {
            setToolTipText(ttoolTip);
            this.name = name;
            imageIconKlein = iimageIconKlein;
            imageIconNormal = iimageIconNormal;
            setOpaque(false);
            setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
            setHorizontalTextPosition(SwingConstants.CENTER);
            setVerticalTextPosition(SwingConstants.BOTTOM);
            buttonList.add(this);
        }

        @Override
        public void setAction(Action a) {
            super.setAction(a);
            this.name = (String) a.getValue(Action.NAME);
            setHideActionText(true);
        }

        void setIconSize() {
            if (nrIconKlein != null) {
                if (Boolean.parseBoolean(MVConfig.get(nrIconKlein))) {
                    this.setIcon(imageIconKlein);
                } else {
                    this.setIcon(imageIconNormal);
                }
            }
        }
    }

    private class BeobMausToolBar extends MouseAdapter {

        private final JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        private final JMenuItem itemReset = new JMenuItem("zurücksetzen");
        private JCheckBoxMenuItem[] checkBoxMenuItems;

        public BeobMausToolBar() {
            if (nrIconKlein != null) {
                itemKlein.setSelected(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
            }
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();
            itemKlein.addActionListener(e -> {
                setToolBarIconSize(itemKlein.isSelected());
                Listener.notify(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, ToolBar.class.getSimpleName() + state);
            });
            jPopupMenu.add(itemKlein);
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

            // Spalten ein-ausschalten
            checkBoxMenuItems = new JCheckBoxMenuItem[buttonList.size()];
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                checkBoxMenuItems[i] = null;
                checkBoxMenuItems[i] = new JCheckBoxMenuItem(buttonList.get(i).name);
                if (checkBoxMenuItems[i] != null) {
                    checkBoxMenuItems[i] = new JCheckBoxMenuItem(buttonList.get(i).name);
                    checkBoxMenuItems[i].setIcon(buttonList.get(i).imageIconKlein);
                    checkBoxMenuItems[i].setSelected(buttonList.get(i).anzeigen);
                    checkBoxMenuItems[i].addActionListener(e -> {
                        setButtonList();
                        writeToolBarButtonStates();
                    });
                    jPopupMenu.add(checkBoxMenuItems[i]);
                }
            }
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            itemReset.addActionListener(e -> {
                resetToolbar();
                writeToolBarButtonStates();
                Listener.notify(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, ToolBar.class.getSimpleName() + state);
            });
            jPopupMenu.add(itemReset);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private void setButtonList() {
            if (checkBoxMenuItems == null) {
                return;
            }
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                if (checkBoxMenuItems[i] == null) {
                    continue;
                }
                buttonList.get(i).anzeigen = checkBoxMenuItems[i].isSelected();
                buttonList.get(i).setVisible(checkBoxMenuItems[i].isSelected());
            }
            setToolbar();
        }

        private void resetToolbar() {
            if (checkBoxMenuItems == null) {
                return;
            }
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                if (checkBoxMenuItems[i] == null) {
                    continue;
                }
                buttonList.get(i).anzeigen = true;
                buttonList.get(i).setVisible(true);
            }
            setToolbar();
            setToolBarIconSize(false);
        }

    }
}
