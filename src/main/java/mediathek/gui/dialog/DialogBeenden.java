package mediathek.gui.dialog;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.file.GetFile;
import mediathek.javafx.AppTerminationIndefiniteProgress;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@SuppressWarnings("serial")
public class DialogBeenden extends JDialog {
    private static final String CANCEL_AND_TERMINATE_PROGRAM = "Downloads abbrechen und Programm beenden";
    private static final String WAIT_FOR_DOWNLOADS_AND_TERMINATE = "Auf Abschluß aller Downloads warten, danach beenden";
    private static final String WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE = "Nur auf bereits laufende Downloads warten, danach beenden";
    private static final String DONT_TERMINATE = "Programm nicht beenden";
    private final JFrame parent;

    /**
     * Indicates whether the application can terminate.
     */
    private boolean applicationCanTerminate = false;
    private boolean onlyRunningDownloads = false;
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

    public DialogBeenden(JFrame pparent) {
        super(pparent, true);
        initComponents();
        this.parent = pparent;

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

        jButtonHilfe.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHilfe.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_BEENDEN)).setVisible(true));
        jButtonHilfe.setEnabled(false);
        cbShutdownComputer.setEnabled(false);

        comboActions.addActionListener(e -> {
            final String strSelectedItem = (String) Objects.requireNonNull(comboActions.getSelectedItem());
            switch (strSelectedItem) {
                case WAIT_FOR_DOWNLOADS_AND_TERMINATE:
                case WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE:
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
        });

        cbShutdownComputer.addActionListener(e -> shutdown = cbShutdownComputer.isSelected());

        btnContinue.addActionListener(e -> {
            final String strSelectedItem = (String) Objects.requireNonNull(comboActions.getSelectedItem());
            switch (strSelectedItem) {
                case WAIT_FOR_DOWNLOADS_AND_TERMINATE:
                    waitUntilDownloadsHaveFinished();
                    break;
                case WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE:
                    onlyRunningDownloads = true;
                    waitUntilDownloadsHaveFinished();
                    break;

                case CANCEL_AND_TERMINATE_PROGRAM:
                    applicationCanTerminate = true;
                    dispose();
                    break;

                case DONT_TERMINATE:
                    applicationCanTerminate = false;
                    dispose();
                    break;
            }
        });

        btnCancel.addActionListener(e -> escapeHandler());

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

    public void setComboWaitAndTerminate() {
        comboActions.setSelectedItem(WAIT_FOR_DOWNLOADS_AND_TERMINATE);
        cbShutdownComputer.setSelected(true);
        shutdown = true;
    }

    /**
     * Create the ComboBoxModel for user selection.
     *
     * @return The model with all valid user actions.
     */
    private DefaultComboBoxModel<String> getComboBoxModel() {
        return new DefaultComboBoxModel<>(new String[]{CANCEL_AND_TERMINATE_PROGRAM, WAIT_FOR_DOWNLOADS_AND_TERMINATE, WAIT_FOR_RUNNING_DOWNLOADS_AND_TERMINATE, DONT_TERMINATE});
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

        AppTerminationIndefiniteProgress progPanel = new AppTerminationIndefiniteProgress(isShutdownRequested());
        panel.add(progPanel, BorderLayout.CENTER);

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

        if (onlyRunningDownloads) {
            MediathekGui.ui().tabDownloads.stopAllWaitingDownloads();
            onlyRunningDownloads = false;
        }

        downloadMonitorWorker = new SwingWorker<>() {
            @Override
            protected Void doInBackground() throws Exception {
                while ((Daten.getInstance().getListeDownloads().unfinishedDownloads() > 0) && !isCancelled())
                    TimeUnit.SECONDS.sleep(1);

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
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jLabel1 = new JLabel();
        comboActions = new JComboBox<>();
        btnContinue = new JButton();
        cbShutdownComputer = new JCheckBox();
        btnCancel = new JButton();
        jButtonHilfe = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("MediathekView beenden"); //NON-NLS
        setResizable(false);
        var contentPane = getContentPane();

        //---- jLabel1 ----
        jLabel1.setText("<html>Es sind noch nicht alle Downloads fertig.<br>Wie m\u00f6chten Sie fortfahren?</html>"); //NON-NLS

        //---- comboActions ----
        comboActions.setModel(getComboBoxModel());

        //---- btnContinue ----
        btnContinue.setText("Weiter"); //NON-NLS

        //---- cbShutdownComputer ----
        cbShutdownComputer.setText("Rechner herunterfahren"); //NON-NLS

        //---- btnCancel ----
        btnCancel.setText("Abbrechen"); //NON-NLS

        //---- jButtonHilfe ----
        jButtonHilfe.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); //NON-NLS
        jButtonHilfe.setToolTipText("Hilfe anzeigen"); //NON-NLS

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jLabel1, GroupLayout.DEFAULT_SIZE, 521, Short.MAX_VALUE)
                        .addComponent(comboActions, GroupLayout.DEFAULT_SIZE, 0, Short.MAX_VALUE)
                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                            .addGap(0, 0, Short.MAX_VALUE)
                            .addComponent(jButtonHilfe)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnCancel)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(btnContinue))
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(cbShutdownComputer)
                            .addGap(0, 351, Short.MAX_VALUE)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jLabel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(comboActions, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(cbShutdownComputer)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                        .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(btnContinue)
                            .addComponent(btnCancel))
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
    // End of variables declaration//GEN-END:variables
}
