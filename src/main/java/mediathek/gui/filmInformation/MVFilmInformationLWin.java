/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.gui.filmInformation;

import mSearch.daten.DatenFilm;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.EscBeenden;
import mediathek.tool.GuiFunktionen;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.net.URISyntaxException;

@SuppressWarnings("serial")
public class MVFilmInformationLWin extends JDialog implements IFilmInformation {
    private JXHyperlink lblUrlThemaField;
    private JXHyperlink lblUrlSubtitle;
    private JTextArea textAreaBeschreibung;
    private JLabel jLabelFilmNeu;
    private JLabel jLabelFilmHD;
    private JLabel jLabelFilmUT;
    private final JLabel[] labelArrNames = new JLabel[DatenFilm.MAX_ELEM];
    private final JTextField[] txtArrCont = new JTextField[DatenFilm.MAX_ELEM];
    private DatenFilm aktFilm = new DatenFilm();
    private final JFrame parent;
    private static final ImageIcon ja_sw_16 = Icons.ICON_DIALOG_EIN_SW;
    static Point mouseDownCompCoords;
    private final JDialog dialog;

    public MVFilmInformationLWin(JFrame owner) {
        super(MVConfig.getBool(MVConfig.Configs.SYSTEM_FILM_INFO_TOP) ? owner : null, false);
        initComponents();

        dialog = this;
        parent = owner;
        setTitle("Filminformation");

        for (int i = 0; i < DatenFilm.MAX_ELEM; ++i) {
            labelArrNames[i] = new JLabel(DatenFilm.COLUMN_NAMES[i] + ":");
            labelArrNames[i].setHorizontalAlignment(SwingConstants.RIGHT);
            labelArrNames[i].setDoubleBuffered(true);
            txtArrCont[i] = new JTextField("");
            txtArrCont[i].setEditable(false);
            txtArrCont[i].setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
            txtArrCont[i].setDoubleBuffered(true);
        }

        Dimension size = new Dimension(500, 600);//w,h
        this.setSize(size);

        setExtra(jPanelExtra);
        jPanelExtra.addMouseListener(new BeobMaus());

        new EscBeenden(this) {
            @Override
            public void beenden_(JDialog d) {
                d.dispose();
            }
        };

        mouseDownCompCoords = null;
        jPanelExtra.addMouseListener(new MouseListener() {
            @Override
            public void mouseReleased(MouseEvent e) {
                mouseDownCompCoords = null;
            }

            @Override
            public void mousePressed(MouseEvent e) {
                mouseDownCompCoords = e.getPoint();
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseClicked(MouseEvent e) {
            }
        });
        jPanelExtra.addMouseMotionListener(new MouseMotionListener() {
            @Override
            public void mouseMoved(MouseEvent e) {
            }

            @Override
            public void mouseDragged(MouseEvent e) {
                Point currCoords = e.getLocationOnScreen();
                setLocation(currCoords.x - mouseDownCompCoords.x, currCoords.y - mouseDownCompCoords.y);
            }
        });
        GuiFunktionen.setDialogDecorated(dialog, jPanelOut, MVConfig.getBool(MVConfig.Configs.SYSTEM_FILM_INFO_DECORATED));
    }

    private void setExtra(JPanel jPanel) {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        lblUrlThemaField = new JXHyperlink();
        lblUrlThemaField.setDoubleBuffered(true);
        lblUrlThemaField.setMinimumSize(new Dimension(10, 10));
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.addMouseListener(new BeobMausUrl(lblUrlThemaField));

        lblUrlSubtitle = new JXHyperlink();
        lblUrlSubtitle.setDoubleBuffered(true);
        lblUrlSubtitle.setMinimumSize(new Dimension(10, 10));
        try {
            lblUrlSubtitle.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlSubtitle.addMouseListener(new BeobMausUrl(lblUrlSubtitle));

        jLabelFilmNeu = new JLabel();
        jLabelFilmNeu.setOpaque(false);
        jLabelFilmNeu.setVisible(false);
        jLabelFilmNeu.setIcon(ja_sw_16);

        jLabelFilmHD = new JLabel();
        jLabelFilmHD.setOpaque(false);
        jLabelFilmHD.setVisible(false);
        jLabelFilmHD.setIcon(ja_sw_16);

        jLabelFilmUT = new JLabel();
        jLabelFilmUT.setOpaque(false);
        jLabelFilmUT.setVisible(false);
        jLabelFilmUT.setIcon(ja_sw_16);

        textAreaBeschreibung = new JTextArea();
        textAreaBeschreibung.setDoubleBuffered(true);
        textAreaBeschreibung.setLineWrap(true);
        textAreaBeschreibung.setWrapStyleWord(true);
        textAreaBeschreibung.setRows(4);
        textAreaBeschreibung.setOpaque(false);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weighty = 0;
        jPanel.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < labelArrNames.length; ++i) {
            if (i == DatenFilm.FILM_URL_RTMP
                    || i == DatenFilm.FILM_URL_AUTH
                    || i == DatenFilm.FILM_URL_HD
                    || i == DatenFilm.FILM_URL_RTMP_HD
                    || i == DatenFilm.FILM_URL_KLEIN
                    || i == DatenFilm.FILM_URL_RTMP_KLEIN
                    || i == DatenFilm.FILM_ABSPIELEN
                    || i == DatenFilm.FILM_AUFZEICHNEN
                    || i == DatenFilm.FILM_DATUM_LONG
                    || i == DatenFilm.FILM_URL_HISTORY
                    || i == DatenFilm.FILM_REF) {
                continue;
            }
            c.gridy = zeile;
            addLable(i, gridbag, c, jPanel);
            ++zeile;
        }

        // zum zusammenschieben
        c.weightx = 0;
        c.gridx = 0;
        c.weighty = 1;
        c.gridy = zeile;
        JLabel label = new JLabel("");
        gridbag.setConstraints(label, c);
        jPanel.add(label);
    }

    private void addLable(int i, GridBagLayout gridbag, GridBagConstraints c, JPanel panel) {
        c.gridx = 0;
        c.weightx = 0;
        gridbag.setConstraints(labelArrNames[i], c);
        panel.add(labelArrNames[i]);
        c.gridx = 1;
        c.weightx = 1;
        switch (i) {
            case DatenFilm.FILM_WEBSEITE:
                gridbag.setConstraints(lblUrlThemaField, c);
                panel.add(lblUrlThemaField);
                break;
            case DatenFilm.FILM_URL_SUBTITLE:
                gridbag.setConstraints(lblUrlSubtitle, c);
                panel.add(lblUrlSubtitle);
                break;
            case DatenFilm.FILM_BESCHREIBUNG:
                JScrollPane sp = new JScrollPane();
                sp.setMinimumSize(new Dimension(10, 100));
                sp.setViewportView(textAreaBeschreibung);
                gridbag.setConstraints(sp, c);
                panel.add(sp);
                break;
            case DatenFilm.FILM_NEU:
                gridbag.setConstraints(jLabelFilmNeu, c);
                panel.add(jLabelFilmNeu);
                break;
            case DatenFilm.FILM_HD:
                gridbag.setConstraints(jLabelFilmHD, c);
                panel.add(jLabelFilmHD);
                break;
            case DatenFilm.FILM_UT:
                gridbag.setConstraints(jLabelFilmUT, c);
                panel.add(jLabelFilmUT);
                break;
            default:
                gridbag.setConstraints(txtArrCont[i], c);
                panel.add(txtArrCont[i]);
                break;
        }
    }

    @Override
    public void showInfo() {
        setAktFilm();
        super.setVisible(true);
    }

    @Override
    public void updateCurrentFilm(DatenFilm film) {
        aktFilm = film;
        if (this.isVisible()) {
            setAktFilm();
        }
    }

    private void setAktFilm() {
        if (aktFilm == null) {
            for (JTextField aTxtArrCont : txtArrCont) {
                aTxtArrCont.setText("");
            }
            textAreaBeschreibung.setText(" ");
            lblUrlThemaField.setText("");
            lblUrlSubtitle.setText("");
            jLabelFilmNeu.setVisible(false);
            jLabelFilmHD.setVisible(false);
            jLabelFilmUT.setVisible(false);
        } else {
            for (int i = 0; i < txtArrCont.length; ++i) {
                txtArrCont[i].setText(aktFilm.arr[i]);
            }
            if (aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG].equals("")) {
                // sonst müsste die Größe gesetzt werden
                textAreaBeschreibung.setText(" ");
            } else {
                textAreaBeschreibung.setText(aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG]);
            }
            lblUrlThemaField.setText(aktFilm.arr[DatenFilm.FILM_WEBSEITE]);
            lblUrlSubtitle.setText(aktFilm.getUrlSubtitle());
            jLabelFilmNeu.setVisible(aktFilm.isNew());
            jLabelFilmHD.setVisible(aktFilm.isHD());
            jLabelFilmUT.setVisible(aktFilm.hasUT());
        }
        this.repaint();
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }

    private class BeobMaus extends MouseAdapter {

        JCheckBox cbkTop = new JCheckBox("Immer im Vordergrund");
        JCheckBox cbkBorder = new JCheckBox("Rand anzeigen");
        JMenuItem itemClose = new JMenuItem("Ausblenden");

        public BeobMaus() {
            cbkTop.setSelected(MVConfig.getBool(MVConfig.Configs.SYSTEM_FILM_INFO_TOP));
            cbkTop.addActionListener(l -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILM_INFO_TOP, Boolean.toString(cbkTop.isSelected()));
                GuiFunktionen.setParent(dialog, MVConfig.getBool(MVConfig.Configs.SYSTEM_FILM_INFO_TOP) ? parent : null);
            });

            cbkBorder.setSelected(MVConfig.getBool(MVConfig.Configs.SYSTEM_FILM_INFO_DECORATED));
            cbkBorder.addActionListener(l -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILM_INFO_DECORATED, Boolean.toString(cbkBorder.isSelected()));
                GuiFunktionen.setDialogDecorated(dialog, jPanelOut, MVConfig.getBool(MVConfig.Configs.SYSTEM_FILM_INFO_DECORATED));
                //setDialogBorder();
            });
            itemClose.addActionListener(l -> dispose());
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

            jPopupMenu.add(cbkTop);
            jPopupMenu.add(cbkBorder);
            jPopupMenu.addSeparator();
            jPopupMenu.add(itemClose);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanelOut = new javax.swing.JPanel();
        jPanelExtra = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jPanelExtra.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 477, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 650, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelOutLayout = new javax.swing.GroupLayout(jPanelOut);
        jPanelOut.setLayout(jPanelOutLayout);
        jPanelOutLayout.setHorizontalGroup(
            jPanelOutLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelOutLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanelOutLayout.setVerticalGroup(
            jPanelOutLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelOutLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelOut, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelOut, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JPanel jPanelOut;
    // End of variables declaration//GEN-END:variables
}
