package mediathek.gui.toolbar;

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
public final class AboToolBar extends ToolBarBase {
    private final MVConfig.Configs nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_ABO;
    private final BeobMausToolBar beobMausToolBar = new BeobMausToolBar();

    public AboToolBar(Daten ddaten) {
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
        startupAbo();

        add(filler__10);
        // Icons
        setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        loadVisible();
        addMouseListener(beobMausToolBar);
    }

    private void startupAbo() {
        MVButton jButtonAbosEinschalten = new MVButton("Abos einschalten", "Abos einschalten", Icons.ICON_TOOLBAR_ABO_EIN_GR, Icons.ICON_TOOLBAR_ABO_EIN_KL);
        jButtonAbosEinschalten.addActionListener(e -> MediathekGui.ui().tabAbos.einAus(true));
        buttonList.add(jButtonAbosEinschalten);

        MVButton jButtonAbosAusschalten = new MVButton("Abos ausschalten", "Abos ausschalten", Icons.ICON_TOOLBAR_ABO_AUS_GR, Icons.ICON_TOOLBAR_ABO_AUS_KL);
        jButtonAbosAusschalten.addActionListener(e -> MediathekGui.ui().tabAbos.einAus(false));
        buttonList.add(jButtonAbosAusschalten);

        MVButton jButtonAbosLoeschen = new MVButton("Abos löschen", "Abos löschen", Icons.ICON_TOOLBAR_ABO_DEL_GR, Icons.ICON_TOOLBAR_ABO_DEL_KL);
        jButtonAbosLoeschen.addActionListener(e -> MediathekGui.ui().tabAbos.loeschen());
        buttonList.add(jButtonAbosLoeschen);

        MVButton jButtonAboAendern = new MVButton("Abos ändern", "Abos ändern", Icons.ICON_TOOLBAR_ABO_CONFIG_GR, Icons.ICON_TOOLBAR_ABO_CONFIG_KL);
        jButtonAboAendern.addActionListener(e -> MediathekGui.ui().tabAbos.aendern());
        buttonList.add(jButtonAboAendern);

        this.add(filler__10);
        this.add(jButtonAbosEinschalten);
        this.add(jButtonAbosAusschalten);
        this.add(jButtonAbosLoeschen);
        this.add(jButtonAboAendern);
        this.add(filler__trenner);

        // Button Filter
        JButton jButtonFilterPanel = createFilterButton();
        this.add(jButtonFilterPanel);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, AboToolBar.class.getName());
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
