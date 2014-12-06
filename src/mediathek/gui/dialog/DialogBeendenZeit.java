/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    Copyright (C) 2014   Christian F.
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialog;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerDateModel;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import org.jdesktop.swingx.JXBusyLabel;

public class DialogBeendenZeit extends JDialog {

    private static final String WAIT_FOR_DOWNLOADS_AND_TERMINATE = "Auf Abschluß aller Downloads warten und danach Programm beenden";
    private static final String WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM = "Auf Abschluß aller Downloads warten, Programm danach NICHT beenden";
    private static final String DONT_START = "Downloads nicht starten";
    private final JFrame parent;
    /**
     * Indicates whether the application can terminate.
     */
    private boolean applicationCanTerminate = false;
    /**
     * Indicate whether computer should be shut down.
     */
    private boolean shutdown = false;
    /**
     * JPanel for displaying the glassPane with the busy indicator label.
     */
    private JPanel glassPane = null;
    /**
     * The download monitoring {@link javax.swing.SwingWorker}.
     */
    private SwingWorker<Void, Void> downloadMonitorWorker = null;

    /**
     * Return whether the user still wants to terminate the application.
     *
     * @return true if the app should continue to terminate.
     */
    public boolean applicationCanTerminate() {
        return applicationCanTerminate;
    }

    public DialogBeendenZeit(JFrame pparent) {
        super(pparent, true);
        initComponents();
        this.parent = pparent;

        if (parent != null) {
            setLocationRelativeTo(parent);
        }
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                escapeHandler();
            }
        };

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                escapeHandler();
            }
        });

        SpinnerDateModel model = new SpinnerDateModel();
        jSpinnerTime.setModel(model);
        jSpinnerTime.addChangeListener(new ChangeListener() {

            @Override
            public void stateChanged(ChangeEvent e) {

            }
        });

        // set date format
        SimpleDateFormat format = ((JSpinner.DateEditor) jSpinnerTime.getEditor()).getFormat();
        //format.setTimeZone(TimeZone.getTimeZone("Europe/London"));
        format.applyPattern("HH:mm");

        model.setValue(new Date());

        jButtonHilfe.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonHilfe.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_BEENDEN)).setVisible(true);
            }
        });
        jButtonHilfe.setEnabled(false);
        cbShutdownComputer.setEnabled(false);

        comboActions.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String strSelectedItem = (String) comboActions.getSelectedItem();
                switch (strSelectedItem) {
                    case WAIT_FOR_DOWNLOADS_AND_TERMINATE:
                        jButtonHilfe.setEnabled(true);
                        cbShutdownComputer.setEnabled(true);
                        break;

                    default:
                        jButtonHilfe.setEnabled(false);
                        cbShutdownComputer.setEnabled(false);
                        //manually reset shutdown state
                        jButtonHilfe.setEnabled(false);
                        cbShutdownComputer.setSelected(false);
                        shutdown = false;
                        break;
                }
            }
        });

        cbShutdownComputer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                shutdown = cbShutdownComputer.isSelected();
            }
        });

        btnContinue.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final String strSelectedItem = (String) comboActions.getSelectedItem();
                switch (strSelectedItem) {
                    case WAIT_FOR_DOWNLOADS_AND_TERMINATE:
                        waitUntilDownloadsHaveFinished();
                        break;

                    case WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM:
                        applicationCanTerminate = false;
                        waitUntilDownloadsHaveFinished();
                        break;

                    case DONT_START:
                        applicationCanTerminate = false;
                        dispose();
                        break;
                }
            }
        });

        btnCancel.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                escapeHandler();
            }
        });

        pack();

        getRootPane().setDefaultButton(btnContinue);
    }

    /**
     * Does the user want to shutdown the computer?
     *
     * @return true if shutdown is wanted.
     */
    public boolean isShutdownRequested() {
        return shutdown;
    }

    /**
     * Create the ComboBoxModel for user selection.
     *
     * @return The model with all valid user actions.
     */
    private DefaultComboBoxModel<String> getComboBoxModel() {
        return new DefaultComboBoxModel<>(new String[]{ WAIT_FOR_DOWNLOADS_AND_TERMINATE,WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM, DONT_START});
    }

    /**
     * This will reset all necessary variables to default and cancel app termination.
     */
    private void escapeHandler() {
        if (downloadMonitorWorker != null) {
            downloadMonitorWorker.cancel(true);
        }

        if (glassPane != null) {
            glassPane.setVisible(false);
        }

        applicationCanTerminate = false;
        dispose();
    }

    /**
     * Create the glassPane which will be used as an overlay for the dialog while we are waiting for downloads.
     *
     * @return The {@link javax.swing.JPanel} for the glassPane.
     */
    private JPanel createGlassPane() {
        String strMessage = "<html>Warte auf Abschluss der Downloads...";
        if (isShutdownRequested()) {
            strMessage += "<br><b>Der Rechner wird danach heruntergefahren.</b>";
        }
        strMessage += "<br>Sie können den Vorgang mit Escape abbrechen.</html>";

        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout(5, 5));
        JXBusyLabel lbl = new JXBusyLabel();
        lbl.setText(strMessage);
        lbl.setBusy(true);
        lbl.setVerticalAlignment(SwingConstants.CENTER);
        lbl.setHorizontalAlignment(SwingConstants.CENTER);
        panel.add(lbl, BorderLayout.CENTER);

        return panel;
    }

    /**
     * Handler which will wait untill all downloads have finished.
     * Uses a {@link javax.swing.SwingWorker} to properly handle EDT stuff.
     */
    private void waitUntilDownloadsHaveFinished() {
        glassPane = createGlassPane();
        setGlassPane(glassPane);
        glassPane.setVisible(true);

        downloadMonitorWorker = new SwingWorker<Void, Void>() {
            @Override
            protected Void doInBackground() throws Exception {
                while ((Daten.listeDownloads.nochNichtFertigeDownloads() > 0) && !isCancelled()) {
                    Thread.sleep(1000);
                }

                return null;
            }

            @Override
            protected void done() {
                applicationCanTerminate = true;
                glassPane.setVisible(false);
                dispose();
                downloadMonitorWorker = null;
            }
        };
        downloadMonitorWorker.execute();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        comboActions = new javax.swing.JComboBox<String>();
        btnContinue = new javax.swing.JButton();
        cbShutdownComputer = new javax.swing.JCheckBox();
        btnCancel = new javax.swing.JButton();
        jButtonHilfe = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jSpinnerTime = new javax.swing.JSpinner();
        jLabel3 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("MediathekView beenden");
        setResizable(false);

        jLabel1.setText("<html>Wie möchten Sie fortfahren<br>\nwenn alle Downloads fertig sind?</html>");

        comboActions.setModel(getComboBoxModel());

        btnContinue.setText("Weiter");

        cbShutdownComputer.setText("Rechner herunterfahren");

        btnCancel.setText("Abbrechen");

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N

        jLabel2.setText("alle Downloads starten um: ");

        jLabel3.setText("Uhr");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, 616, Short.MAX_VALUE)
                    .addComponent(comboActions, 0, 0, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(cbShutdownComputer)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonHilfe))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(btnCancel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(btnContinue))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jSpinnerTime, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel3)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jSpinnerTime, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel3))
                .addGap(18, 18, Short.MAX_VALUE)
                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(comboActions, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cbShutdownComputer)
                    .addComponent(jButtonHilfe))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(btnContinue)
                    .addComponent(btnCancel))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnCancel;
    private javax.swing.JButton btnContinue;
    private javax.swing.JCheckBox cbShutdownComputer;
    private javax.swing.JComboBox<String> comboActions;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JSpinner jSpinnerTime;
    // End of variables declaration//GEN-END:variables
}
