package mediathek.gui;

import com.explodingpixels.macwidgets.BottomBar;
import com.explodingpixels.macwidgets.BottomBarSize;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.EnumMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;

/**
 * User: crystalpalace1977
 * Date: 02.03.13
 * Time: 19:54
 */
public final class MVStatusBar extends JPanel {

    private boolean stopTimer = false;
    private final EnumMap<MVStatusBar.StatusbarIndex, String> displayListForLeftLabel = new EnumMap<>(MVStatusBar.StatusbarIndex.class);
    private MVStatusBar.StatusbarIndex currentIndex = MVStatusBar.StatusbarIndex.NONE;

    private final JLabel lblCenter;
    private final JLabel lblRechts;
    private final JProgressBar progress;
    private final JButton stopButton;
    private final BottomBar bottomBar;

    public MVStatusBar() {
        bottomBar = new BottomBar(BottomBarSize.LARGE);

        lblCenter = new JLabel();
        bottomBar.addComponentToLeft(lblCenter);

        if (Daten.debug) {
            bottomBar.addComponentToCenter(new MVMemoryUsageButton());
        }

        //Progress controls and Filminfo must be packed in a panel
        JPanel progressPanel = new JPanel();
        progressPanel.setBackground(bottomBar.getComponent().getBackground());
        progressPanel.setLayout(new FlowLayout());
        progressPanel.setOpaque(false);

        lblRechts = new JLabel();
        progressPanel.add(lblRechts);

        progress = new JProgressBar();
        progressPanel.add(progress);

        stopButton = new JButton();
        stopButton.setIcon(GetIcon.getProgramIcon("close.png"));

        stopButton.setToolTipText("Abbrechen");
        stopButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.setStop(true);
            }
        });

        progressPanel.add(stopButton);
        bottomBar.addComponentToRight(progressPanel);

        hideProgressIndicators();
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_TIMER, MVStatusBar.class.getSimpleName()) {
            @Override
            public void ping() {
                try {
                    if (!stopTimer) {
                        setInfoRechts();
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(936251087, ex);
                }
            }
        });
    }

    public enum StatusbarIndex {

        NONE, FILME, DOWNLOAD, ABO
    }

    public JComponent getComponent() {
        return bottomBar.getComponent();
    }

    /**
     * Hide the progress bar indicator and stop button
     */
    public void hideProgressIndicators() {
        stopTimer = false;
        progress.setVisible(false);
        stopButton.setVisible(false);
        lblRechts.setVisible(true);
        setInfoRechts();
    }

    public void updateProgressBar(MSListenerFilmeLadenEvent event) {
        stopTimer = true;
        if (!progress.isVisible()) {
            progress.setVisible(true);
            stopButton.setVisible(true);
        }
        if (event.max == 0 || event.progress == event.max) {
            progress.setIndeterminate(true);
            progress.setMaximum(0);
            progress.setMinimum(0);
            progress.setValue(0);
            progress.setStringPainted(true);
            progress.setString(event.text);
        } else {
            progress.setIndeterminate(false);
            progress.setMaximum(event.max);
            progress.setMinimum(0);
            progress.setValue(event.progress);
            progress.setStringPainted(true);
            progress.setString(event.text);
        }
        if (Daten.debug) {
            lblRechts.setText(GuiFunktionen.textLaenge(60, event.senderUrl, true /* mitte */, true /*addVorne*/));
        } else {
            lblRechts.setVisible(false);
        }
    }

    private void setInfoRechts() {
        // Text rechts: alter/neuladenIn anzeigen
        String strText = "Filmliste erstellt: ";
        strText += Daten.listeFilme.genDate();
        strText += " Uhr  ";

        final int sekunden = Daten.listeFilme.getAge();

        if (sekunden != 0) {
            strText += "||  Alter: ";
            final int minuten = sekunden / 60;
            String strSekunde = String.valueOf(sekunden % 60);
            String strMinute = String.valueOf(minuten % 60);
            String strStunde = String.valueOf(minuten / 60);
            if (strSekunde.length() < 2) {
                strSekunde = "0" + strSekunde;
            }
            if (strMinute.length() < 2) {
                strMinute = "0" + strMinute;
            }
            if (strStunde.length() < 2) {
                strStunde = "0" + strStunde;
            }
            strText += strStunde + ":" + strMinute + ":" + strSekunde + " ";
        }
        // Infopanel setzen
        lblRechts.setText(strText);
    }

    public void setTextLeft(StatusbarIndex i, String text) {
        displayListForLeftLabel.put(i, text);
        setIndexForCenterDisplay(currentIndex);
    }

    /**
     * Wähle den Textstring aus dem Index {@code i}, der im zentralen Textfeld dargestellt werden soll.
     *
     * @param i Index, für den ein Text dargestellt werden soll.
     */
    public void setIndexForCenterDisplay(StatusbarIndex i) {
        currentIndex = i;
        String displayString = displayListForLeftLabel.get(i);
        lblCenter.setText(displayString);
    }
}
