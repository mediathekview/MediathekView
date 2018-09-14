package mediathek.gui.toolbar;

import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.messages.ToolBarIconSizeChangedEvent;
import net.engio.mbassy.listener.Handler;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

@SuppressWarnings("serial")
public final class DownloadToolBar extends ToolBarBase {
    private final MVConfig.Configs nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_DOWNLOAD;
    private final Daten daten;
    private final BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    private MVButton jButtonDownloadAktualisieren = null;

    public DownloadToolBar(Daten ddaten) {
        daten = ddaten;

        startup();
        setButtonVisibility();

        ddaten.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleChangeIconSizeEvent(ToolBarIconSizeChangedEvent e) {
        SwingUtilities.invokeLater(() -> setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein))));
    }

    private void startup() {
        setFloatable(false);
        startupDownload();

        add(filler__10);
        // Icons
        setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        loadVisible();
        addMouseListener(beobMausToolBar);
    }

    private void startupDownload() {
        installFilmlistListener();

        MVButton jButtonInfo = new MVButton("Filminformation anzeigen", "Filminformation anzeigen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_INFO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_INFO_KL);
        jButtonInfo.addActionListener(e -> MediathekGui.ui().getFilmInfoDialog().showInfo());
        buttonList.add(jButtonInfo);

        jButtonDownloadAktualisieren = new MVButton("Liste der Downloads aktualisieren", "Liste der Downloads aktualisieren", Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_GR, Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_KL);
        jButtonDownloadAktualisieren.addActionListener(e -> MediathekGui.ui().tabDownloads.aktualisieren());
        buttonList.add(jButtonDownloadAktualisieren);

        MVButton jButtonDownloadAlleStarten = new MVButton("alle Downloads starten", "alle Downloads starten", Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_GR, Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_KL);
        jButtonDownloadAlleStarten.addActionListener(e -> MediathekGui.ui().tabDownloads.starten(true));
        buttonList.add(jButtonDownloadAlleStarten);

        MVButton jButtonDownloadFilmStarten = new MVButton("Film Starten", "gespeicherten Film abspielen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_KL);
        jButtonDownloadFilmStarten.addActionListener(e -> MediathekGui.ui().tabDownloads.filmAbspielen());
        buttonList.add(jButtonDownloadFilmStarten);

        MVButton jButtonDownloadZurueckstellen = new MVButton("Downloads zurückstellen", "Downloads zurückstellen", Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_KL);
        jButtonDownloadZurueckstellen.addActionListener(e -> MediathekGui.ui().tabDownloads.zurueckstellen());
        buttonList.add(jButtonDownloadZurueckstellen);

        MVButton jButtonDownloadLoeschen = new MVButton("Downloads aus Liste entfernen", "Downloads aus Liste entfernen", Icons.ICON_TOOLBAR_DOWNLOAD_DEL_GR, Icons.ICON_TOOLBAR_DOWNLOAD_DEL_KL);
        jButtonDownloadLoeschen.addActionListener(e -> MediathekGui.ui().tabDownloads.loeschen());
        buttonList.add(jButtonDownloadLoeschen);

        MVButton jButtonDownloadAufraeumen = new MVButton("Liste der Downloads aufräumen", "Liste der Downloads aufräumen", Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_GR, Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_KL);
        jButtonDownloadAufraeumen.addActionListener(e -> MediathekGui.ui().tabDownloads.aufraeumen());
        buttonList.add(jButtonDownloadAufraeumen);

        this.add(filler__10);
        this.add(jButtonInfo);
        this.add(filler__10);
        this.add(jButtonDownloadAktualisieren);
        this.add(jButtonDownloadAlleStarten);
        this.add(jButtonDownloadFilmStarten);
        this.add(jButtonDownloadZurueckstellen);
        this.add(jButtonDownloadLoeschen);
        this.add(jButtonDownloadAufraeumen);
        this.add(filler__trenner);

        // Button Filter
        JButton jButtonFilterPanel = createFilterButton();
        this.add(jButtonFilterPanel);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_DOWNLOAD_FILTER_ANZEIGEN, DownloadToolBar.class.getName());
        });

    }

    private void installFilmlistListener() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                if (jButtonDownloadAktualisieren != null) {
                    jButtonDownloadAktualisieren.setEnabled(false);
                }
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                if (jButtonDownloadAktualisieren != null) {
                    jButtonDownloadAktualisieren.setEnabled(true);
                }
            }
        });
    }

    private void setIcon(boolean klein) {
        MVConfig.add(nrIconKlein, Boolean.toString(klein));
        beobMausToolBar.itemKlein.setSelected(klein);

        setButtonIcons();

        repaint();
    }

    private void loadVisible() {
        String[] b = MVConfig.get(nrToolbar).split(":");
        if (buttonList.size() == b.length) {
            // ansonsten gibt es neue Button: dann alle anzeigen
            for (int i = 0; i < b.length; ++i) {
                buttonList.get(i).setAnzeigen(Boolean.parseBoolean(b[i]));
                buttonList.get(i).setVisible(Boolean.parseBoolean(b[i]));
            }
        }

        setButtonVisibility();
        setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
    }

    private void storeVisible() {
        MVConfig.add(nrToolbar, "");
        for (MVButton b : buttonList) {
            if (!MVConfig.get(nrToolbar).isEmpty()) {
                MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + ':');
            }
            MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + Boolean.toString(b.getAnzeigen()));
        }
    }

    private class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        JMenuItem itemReset = new JMenuItem("zurücksetzen");
        JCheckBoxMenuItem[] checkBoxMenuItems;

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
                setIcon(itemKlein.isSelected());
                Daten.getInstance().getMessageBus().publishAsync(new ToolBarIconSizeChangedEvent());
            });
            jPopupMenu.add(itemKlein);
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

            // Spalten ein-ausschalten
            checkBoxMenuItems = new JCheckBoxMenuItem[buttonList.size()];
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                checkBoxMenuItems[i] = null;
                checkBoxMenuItems[i] = new JCheckBoxMenuItem(buttonList.get(i).getName());
                if (checkBoxMenuItems[i] != null) {
                    checkBoxMenuItems[i] = new JCheckBoxMenuItem(buttonList.get(i).getName());
                    checkBoxMenuItems[i].setIcon(buttonList.get(i).getSmallIcon());
                    checkBoxMenuItems[i].setSelected(buttonList.get(i).getAnzeigen());
                    checkBoxMenuItems[i].addActionListener(e -> {
                        setButtonList();
                        storeVisible();
                    });
                    jPopupMenu.add(checkBoxMenuItems[i]);
                }
            }
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            itemReset.addActionListener(e -> {
                resetToolbar();
                storeVisible();
                Daten.getInstance().getMessageBus().publishAsync(new ToolBarIconSizeChangedEvent());
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
                buttonList.get(i).setAnzeigen(checkBoxMenuItems[i].isSelected());
                buttonList.get(i).setVisible(checkBoxMenuItems[i].isSelected());
            }
            setButtonVisibility();
        }

        private void resetToolbar() {
            if (checkBoxMenuItems == null) {
                return;
            }
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                if (checkBoxMenuItems[i] == null) {
                    continue;
                }
                buttonList.get(i).setAnzeigen(true);
                buttonList.get(i).setVisible(true);
            }
            setButtonVisibility();
            setIcon(false);
        }

    }
}
