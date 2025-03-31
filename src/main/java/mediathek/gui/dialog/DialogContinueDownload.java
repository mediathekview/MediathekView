package mediathek.gui.dialog;

import mediathek.config.Konstanten;
import mediathek.daten.DatenDownload;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.TimeUnit;

public class DialogContinueDownload extends JDialog {
    public enum DownloadResult {

        CANCELLED, CONTINUE, RESTART_WITH_NEW_NAME
    }

    private DownloadResult result;

    private boolean isNewName = false;
    private final MVPanelDownloadZiel mVPanelDownloadZiel;
    private final Timer countdownTimer;
    private final boolean direkterDownload;
    final private JFrame parent;

    public DialogContinueDownload(JFrame pparent, DatenDownload datenDownload, boolean ddirekterDownload) {
        // "weiterführen"
        // true: dann kann der bereits gestartete Download weitergeführt werden, nur direkte Downloads
        // false: dann kann der Download nur neu gestartet werden, die existierende Datei wird gelöscht
        super(pparent, true);
        initComponents();
        this.parent = pparent;
        this.direkterDownload = ddirekterDownload;
        if (!direkterDownload) {
            jButtonWeiter.setText("Überschreiben");
            if (!datenDownload.checkAufrufBauen()) {
                // nur für Downloads mit Programm
                jPanelNewName.setVisible(false);
            }
        }
        mVPanelDownloadZiel = new MVPanelDownloadZiel(null, datenDownload, false);
        jPanelPath.setLayout(new BorderLayout(0, 0));
        jPanelPath.add(mVPanelDownloadZiel, BorderLayout.CENTER);

        if (parent != null) {
            setLocationRelativeTo(parent);
        }

        String dialogText = "<html>Der Film \""
                + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL]
                + "\" existiert bereits.<br>Wie möchten Sie fortfahren?</html>";
        jLabel1.setText(dialogText);

        jButtonNeuerName.addActionListener(e -> {
            isNewName = mVPanelDownloadZiel.setPfadName_geaendert();
            if (!direkterDownload && !isNewName) {
                // dann gibts es nur Überschreiben oder anderer Name, sonst zickt ffmpeg
                MVMessageDialog.showMessageDialog(parent, "Der Dateiname wurde nicht geändert!",
                        "Datei existiert bereits!", JOptionPane.ERROR_MESSAGE);
            } else {
                result = DownloadResult.RESTART_WITH_NEW_NAME;
                beenden();
            }
        });

        jButtonAbbrechen.addActionListener(e -> abbrechen());

        EscapeKeyHandler.installHandler(this, this::abbrechen);

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                abbrechen();
            }
        });
        jButtonWeiter.addActionListener(e -> {
            result = DownloadResult.CONTINUE;
            beenden();
        });

        //start the countdown...
        countdownTimer = new Timer(0, new CountdownAction());
        countdownTimer.setRepeats(true);
        countdownTimer.start();

        pack();

        getRootPane().setDefaultButton(jButtonWeiter);

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentShown(ComponentEvent e) {
                //display dialog slightly lower than normal to prevent inadvertent presses from other dialogs (#686)
                var loc = getLocationOnScreen();
                setLocation(loc.x, loc.y + 70);
            }
        });
    }

    /**
     * Return the result of the user selection made in the dialog.
     *
     * @return A {@link mediathek.gui.dialog.DialogContinueDownload.DownloadResult} result.
     */
    public DownloadResult getResult() {
        return result;
    }

    /**
     * Check if a new name was specified.
     *
     * @return A new name needs to be used.
     */
    public boolean isNewName() {
        return isNewName;
    }

    private void abbrechen() {
        result = DownloadResult.CANCELLED;
        beenden();
    }

    private void beenden() {
        if (countdownTimer != null) {
            countdownTimer.stop();
        }

        dispose();
    }

    /**
     * Implements the countdown based on Swing Timer for automatic placement on EDT.
     */
    private class CountdownAction implements ActionListener {

        private int countdown;

        public CountdownAction() {
            countdown = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.DOWNLOAD_CONTINUATION_TIME, Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            String msg;
            if (countdown > 0) {
                if (!direkterDownload) {
                    msg = String.format("Überschreiben in %ds", countdown);
                } else {
                    msg = String.format("Weiterführen in %ds", countdown);
                }
                jButtonWeiter.setText(msg);

                if (countdownTimer != null) {
                    countdownTimer.setDelay((int) TimeUnit.MILLISECONDS.convert(1, TimeUnit.SECONDS));
                }
            } else {
                result = DownloadResult.CONTINUE;
                beenden();
            }
            countdown--;
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonWeiter = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jPanelNewName = new javax.swing.JPanel();
        jButtonNeuerName = new javax.swing.JButton();
        jPanelPath = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Download weiterführen");

        jButtonWeiter.setText("Weiterführen in XXX");

        jButtonAbbrechen.setText("Abbrechen");

        jPanelNewName.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 2));

        jButtonNeuerName.setText("Mit diesem Namen neu Starten");

        javax.swing.GroupLayout jPanelPathLayout = new javax.swing.GroupLayout(jPanelPath);
        jPanelPath.setLayout(jPanelPathLayout);
        jPanelPathLayout.setHorizontalGroup(
                jPanelPathLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 527, Short.MAX_VALUE)
        );
        jPanelPathLayout.setVerticalGroup(
                jPanelPathLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 124, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelNewNameLayout = new javax.swing.GroupLayout(jPanelNewName);
        jPanelNewName.setLayout(jPanelNewNameLayout);
        jPanelNewNameLayout.setHorizontalGroup(
                jPanelNewNameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelNewNameLayout.createSequentialGroup()
                                .addContainerGap(285, Short.MAX_VALUE)
                                .addComponent(jButtonNeuerName)
                                .addContainerGap())
                        .addComponent(jPanelPath, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanelNewNameLayout.setVerticalGroup(
                jPanelNewNameLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelNewNameLayout.createSequentialGroup()
                                .addComponent(jPanelPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonNeuerName)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel1.setText("<html>Die Filmdatei existiert bereits.<br>Wie möchten Sie forfahren?</html>");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jLabel1)
                                        .addComponent(jPanelNewName, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonAbbrechen)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonWeiter)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 40, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonWeiter)
                                        .addComponent(jButtonAbbrechen))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jPanelNewName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(16, 16, 16))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonNeuerName;
    private javax.swing.JButton jButtonWeiter;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanelNewName;
    private javax.swing.JPanel jPanelPath;
    // End of variables declaration//GEN-END:variables
}
