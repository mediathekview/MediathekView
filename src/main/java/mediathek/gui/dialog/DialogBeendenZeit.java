package mediathek.gui.dialog;

import com.github.lgooddatepicker.components.DateTimePicker;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.daten.DatenDownload;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.GetFile;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.swing.AppTerminationIndefiniteProgress;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class DialogBeendenZeit extends JDialog {
    private static final String WAIT_FOR_DOWNLOADS_AND_TERMINATE = "Auf Abschluß aller Downloads warten und danach Programm beenden";
    private static final String WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM = "Auf Abschluß aller Downloads warten, Programm danach NICHT beenden";
    private static final String DONT_START = "Downloads nicht starten";
    private final ArrayList<DatenDownload> listeDownloadsStarten;
    /**
     * Indicates whether the application can terminate.
     */
    private boolean applicationCanTerminate;
    /**
     * Indicate whether computer should be shut down.
     */
    private boolean shutdown;
    /**
     * JPanel for displaying the glassPane with the busy indicator label.
     */
    private JPanel glassPane;

    private AppTerminationIndefiniteProgress progressPanel;

    /**
     * The download monitoring {@link javax.swing.SwingWorker}.
     */
    private BusyWaitWorker downloadMonitorWorker;

    /**
     * Return whether the user still wants to terminate the application.
     *
     * @return true if the app should continue to terminate.
     */
    public boolean applicationCanTerminate() {
        return applicationCanTerminate;
    }

    /**
     * Does the user want to shutdown the computer?
     *
     * @return true if shutdown is wanted.
     */
    public boolean isShutdownRequested() {
        return shutdown;
    }

    public DialogBeendenZeit(JFrame parent, final ArrayList<DatenDownload> listeDownloadsStarten_) {
        super(parent, true);
        initComponents();

        dateTimePicker.datePicker.setDateToToday();
        var timePicker = dateTimePicker.timePicker;
        timePicker.setTimeToNow();
        //apply 1 hour 1 minute patch
        var selTime = timePicker.getTime();
        selTime = selTime.plusHours(1).plusMinutes(1);
        timePicker.setTime(selTime);


        listeDownloadsStarten = listeDownloadsStarten_;
        if (parent != null) {
            setLocationRelativeTo(parent);
        }

        EscapeKeyHandler.installHandler(this, this::escapeHandler);

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                escapeHandler();
            }
        });

        comboActions.setModel(getComboBoxModel());
        comboActions.addActionListener(_ -> setCbShutdownCoputer());

        jButtonHilfe.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
        jButtonHilfe.addActionListener(_ -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(Konstanten.PFAD_HILFETEXT_BEENDEN)).setVisible(true));
        setCbShutdownCoputer();

        cbShutdownComputer.addActionListener(_ -> shutdown = cbShutdownComputer.isSelected());

        btnContinue.addActionListener(_ -> {
            final String strSelectedItem = Objects.requireNonNull(comboActions.getSelectedItem()).toString();

            switch (strSelectedItem) {
                case WAIT_FOR_DOWNLOADS_AND_TERMINATE -> {
                    applicationCanTerminate = true;
                    waitUntilDownloadsHaveFinished();
                }
                case WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM -> {
                    applicationCanTerminate = false;
                    waitUntilDownloadsHaveFinished();
                }
                case DONT_START -> {
                    applicationCanTerminate = false;
                    dispose();
                }
            }
        });

        btnCancel.addActionListener(_ -> escapeHandler());

        getRootPane().setDefaultButton(btnContinue);
        pack();
    }

    private void setCbShutdownCoputer() {
        final String strSelectedItem = (String) comboActions.getSelectedItem();
        if (WAIT_FOR_DOWNLOADS_AND_TERMINATE.equals(strSelectedItem)) {
            cbShutdownComputer.setEnabled(true);
        } else {
            //manually reset shutdown state
            cbShutdownComputer.setEnabled(false);
            cbShutdownComputer.setSelected(false);
            shutdown = false;
        }
    }

    /**
     * Create the ComboBoxModel for user selection.
     *
     * @return The model with all valid user actions.
     */
    private DefaultComboBoxModel<String> getComboBoxModel() {
        return new DefaultComboBoxModel<>(new String[]{WAIT_FOR_DOWNLOADS_AND_TERMINATE, WAIT_FOR_DOWNLOADS_AND_DONT_TERMINATE_PROGRAM, DONT_START});
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
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout(5, 5));

        progressPanel = new AppTerminationIndefiniteProgress(isShutdownRequested());
        panel.add(progressPanel, BorderLayout.CENTER);

        return panel;
    }

    private void setTextWait() {
        var dt = dateTimePicker.getDateTimePermissive();
        var t = dt.format(DateTimeFormatter.ofPattern("HH:mm"));
        var d = dt.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"));
        progressPanel.setMessage(String.format("Downloads werden am %s um %s gestartet.", d,t));
    }

    /**
     * Handler which will wait untill all downloads have finished.
     * Uses a {@link javax.swing.SwingWorker} to properly handle EDT stuff.
     */
    private void waitUntilDownloadsHaveFinished() {
        glassPane = createGlassPane();
        setGlassPane(glassPane);
        setTextWait();
        glassPane.setVisible(true);

        downloadMonitorWorker = new BusyWaitWorker();
        downloadMonitorWorker.execute();
    }


    private class BusyWaitWorker extends SwingWorker<Void, Void> {
        @Override
        protected Void doInBackground() throws Exception {
            while (LocalDateTime.now().isBefore(dateTimePicker.getDateTimePermissive())) {
                TimeUnit.SECONDS.sleep(1);
            }

            progressPanel.setMessage("Warte auf Abschluss der Downloads...");
            DatenDownload.startenDownloads(listeDownloadsStarten);

            while ((Daten.getInstance().getListeDownloads().unfinishedDownloads() > 0) && !isCancelled()) {
                TimeUnit.SECONDS.sleep(1);
            }

            return null;
        }

        @Override
        protected void done() {
            glassPane.setVisible(false);
            dispose();
            downloadMonitorWorker = null;
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jLabel1 = new JLabel();
        comboActions = new JComboBox<>();
        btnContinue = new JButton();
        cbShutdownComputer = new JCheckBox();
        btnCancel = new JButton();
        jButtonHilfe = new JButton();
        var jLabel2 = new JLabel();
        dateTimePicker = new DateTimePicker();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        setTitle("Zeitverz\u00f6gerter Download-Start"); //NON-NLS
        var contentPane = getContentPane();

        //---- jLabel1 ----
        jLabel1.setText("Wie m\u00f6chten Sie fortfahren wenn alle Downloads fertig sind?"); //NON-NLS

        //---- btnContinue ----
        btnContinue.setText("Weiter"); //NON-NLS

        //---- cbShutdownComputer ----
        cbShutdownComputer.setText("Rechner herunterfahren"); //NON-NLS

        //---- btnCancel ----
        btnCancel.setText("Abbrechen"); //NON-NLS

        //---- jButtonHilfe ----
        jButtonHilfe.setToolTipText("Hilfe anzeigen"); //NON-NLS

        //---- jLabel2 ----
        jLabel2.setText("Alle Downloads starten um: "); //NON-NLS

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jLabel1, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(comboActions, GroupLayout.DEFAULT_SIZE, 0, Short.MAX_VALUE)
                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                            .addGap(0, 0, Short.MAX_VALUE)
                            .addComponent(jButtonHilfe)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnCancel)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnContinue))
                        .addComponent(cbShutdownComputer)
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(jLabel2)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(dateTimePicker, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(dateTimePicker, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel2))
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 94, Short.MAX_VALUE)
                    .addComponent(jLabel1)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addComponent(comboActions, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(cbShutdownComputer)
                            .addGap(18, 18, 18)
                            .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(btnContinue)
                                .addComponent(btnCancel)))
                        .addComponent(jButtonHilfe))
                    .addContainerGap())
        );
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JComboBox<String> comboActions;
    private JButton btnContinue;
    private JCheckBox cbShutdownComputer;
    private JButton btnCancel;
    private JButton jButtonHilfe;
    private DateTimePicker dateTimePicker;
    // End of variables declaration//GEN-END:variables
}
