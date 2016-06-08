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
package mediathek.fx;

import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.WindowConstants;
import mediathek.daten.Daten;
import mediathek.tool.MVConfig;

public class DownloadInfoSwing extends javax.swing.JDialog {

    private JCheckBoxMenuItem menuItem = null;

    public DownloadInfoSwing(java.awt.Frame parent, boolean modal, final JCheckBoxMenuItem menuItem) {
        super(parent, modal);
        initComponents();
        this.menuItem = menuItem;

        this.setTitle("Bandbreite");
        this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                menuItem.setSelected(false);
                toggleVisibility();
            }
        });

        final Dimension dim = this.getSize();
        dim.height = 250;
        dim.width = 300;
        this.setSize(dim);
    }

    public synchronized void startSwing() {
        final JFXPanel jfxPanel = new JFXPanel();
        this.setContentPane(jfxPanel);

        Platform.runLater(() -> {
            final DownloadInfoController dic = new DownloadInfoController();
            dic.initFX(jfxPanel);
            dic.startSearch();
        });
        try {
            this.wait(1000);
        } catch (InterruptedException ex) {
        }
    }

    /**
     * Show/hide bandwidth display. Take also care about the used timer.
     */
    public void toggleVisibility() {
        final boolean isSelected = menuItem.isSelected();
        Daten.mVConfig.add(MVConfig.SYSTEM_BANDWIDTH_MONITOR_VISIBLE, Boolean.toString(menuItem.isSelected()));
        this.setVisible(isSelected);
    }


    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 400, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 300, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
}
