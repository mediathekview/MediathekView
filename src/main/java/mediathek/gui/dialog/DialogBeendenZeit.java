package mediathek.gui.dialog;

import mediathek.config.Daten;
import mediathek.daten.DatenDownload;
import mediathek.file.GetFile;
import mediathek.javafx.AppTerminationIndefiniteProgress;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
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
    private SwingWorker<Void, Void> downloadMonitorWorker;

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

        // set date format
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MINUTE, 1);
        Date startDate = cal.getTime();
        cal.add(Calendar.MINUTE, 60); // würde sagen, das ist ein Bug, geht sonst nicht
        Date now = cal.getTime();
        cal.add(Calendar.DATE, 2);
        Date endDate = cal.getTime();
        SpinnerDateModel model = new SpinnerDateModel(now, startDate, endDate, Calendar.MINUTE);

        jSpinnerTime.setModel(model);
        JSpinner.DateEditor dEditor = new JSpinner.DateEditor(jSpinnerTime, "dd.MM.yyy  HH:mm");
        jSpinnerTime.setEditor(dEditor);

        comboActions.setModel(getComboBoxModel());
        comboActions.addActionListener(e -> setCbShutdownCoputer());

        jButtonHilfe.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
        jButtonHilfe.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_BEENDEN)).setVisible(true));
        setCbShutdownCoputer();

        cbShutdownComputer.addActionListener(e -> shutdown = cbShutdownComputer.isSelected());

        btnContinue.addActionListener(e -> {
            final String strSelectedItem = Objects.requireNonNull(comboActions.getSelectedItem()).toString();

            SimpleDateFormat format = ((JSpinner.DateEditor) jSpinnerTime.getEditor()).getFormat();
            format.applyPattern("HH:mm");
            Date sp = (Date) jSpinnerTime.getValue();
            String strDate = format.format(sp);

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

        btnCancel.addActionListener(e -> escapeHandler());

        pack();

        getRootPane().setDefaultButton(btnContinue);
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
        SimpleDateFormat format = ((JSpinner.DateEditor) jSpinnerTime.getEditor()).getFormat();
        format.applyPattern("dd.MM.yyy  HH:mm");
        Date sp = (Date) jSpinnerTime.getValue();
        String strDate = format.format(sp);

        String strMessage = "Downloads starten: " + strDate;
        progressPanel.setMessage(strMessage);
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

        downloadMonitorWorker = new SwingWorker<>() {
            @Override
            protected Void doInBackground() throws Exception {
                while ((((Date) jSpinnerTime.getValue())).after(new Date())) {
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
        };
        downloadMonitorWorker.execute();
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
        jSpinnerTime = new JSpinner();
        var jLabel3 = new JLabel();

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

        //---- jLabel3 ----
        jLabel3.setText("Uhr"); //NON-NLS

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jLabel1, GroupLayout.DEFAULT_SIZE, 386, Short.MAX_VALUE)
                        .addComponent(comboActions, GroupLayout.DEFAULT_SIZE, 0, Short.MAX_VALUE)
                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                            .addGap(0, 0, Short.MAX_VALUE)
                            .addComponent(jButtonHilfe)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnCancel)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnContinue))
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addGroup(contentPaneLayout.createParallelGroup()
                                .addComponent(cbShutdownComputer)
                                .addGroup(contentPaneLayout.createSequentialGroup()
                                    .addComponent(jLabel2)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jSpinnerTime, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jLabel3)))
                            .addGap(0, 0, Short.MAX_VALUE)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel2)
                        .addComponent(jSpinnerTime, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addComponent(jLabel3))
                    .addGap(18, 18, Short.MAX_VALUE)
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
    private JSpinner jSpinnerTime;
    // End of variables declaration//GEN-END:variables
}
