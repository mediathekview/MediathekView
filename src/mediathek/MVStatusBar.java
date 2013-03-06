package mediathek;

import com.explodingpixels.macwidgets.BottomBar;
import com.explodingpixels.macwidgets.BottomBarSize;
import com.explodingpixels.macwidgets.MacWidgetFactory;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.daten.Daten;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Log;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.EnumMap;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

/**
 * User: crystalpalace1977
 * Date: 02.03.13
 * Time: 19:54
 */
public final class MVStatusBar {

    private boolean stopTimer = false;
    private JLabel lblCenter;
    private JLabel lblRechts;
    private JProgressBar progress;
    private JButton stopButton;
    private BottomBar bottomBar;
    /**
     * This contains all the strings that will be displayed according to selected index.
     */
    private EnumMap<StatusbarIndex, String> displayListForLeftLabel = new EnumMap<StatusbarIndex, String>(StatusbarIndex.class);
    private StatusbarIndex currentIndex = StatusbarIndex.NONE;

    public MVStatusBar() {
        bottomBar = new BottomBar(BottomBarSize.LARGE);
        lblCenter = MacWidgetFactory.createEmphasizedLabel("");
        bottomBar.addComponentToLeft(lblCenter);

        lblRechts = MacWidgetFactory.createEmphasizedLabel("");
        bottomBar.addComponentToRight(lblRechts);

        progress = new JProgressBar();
        bottomBar.addComponentToRight(progress);

        stopButton = new JButton();
        stopButton.setIcon(new ImageIcon(getClass().getResource("/com/explodingpixels/macwidgets/images/close.png")));
        stopButton.setToolTipText("Abbrechen");
        stopButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.setStop();
            }
        });
        bottomBar.addComponentToRight(stopButton);

        hideProgressIndicators();
        new AgeUpdateTimer().start();
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

    public void updateProgressBar(ListenerFilmeLadenEvent event) {
        stopTimer = true;
        if (!progress.isVisible()) {
            progress.setVisible(true);
            stopButton.setVisible(true);
        }
        if (event.max == 0) {
            progress.setIndeterminate(true);
            progress.setMaximum(0);
            progress.setMinimum(0);
            progress.setValue(0);
            progress.setStringPainted(false);
        } else {
            progress.setIndeterminate(false);
            progress.setMaximum(event.max);
            progress.setMinimum(0);
            progress.setValue(event.progress);
            progress.setStringPainted(true);
        }
        if (Daten.debug) {
            lblRechts.setText(GuiFunktionen.textLaenge(60, event.text, true /* mitte */, true /*addVorne*/));
        } else {
            lblRechts.setVisible(false);
        }
    }

    private void setInfoRechts() {
        // Text rechts: alter/neuladenIn anzeigen
        String strText = "Filmliste erstellt: ";
        strText += Daten.listeFilme.erstellt();
        strText += " Uhr  ";

        int sekunden = Daten.listeFilme.alterFilmlisteSek();

        if (sekunden != 0) {
            strText += "||  Alter: ";
            int minuten = sekunden / 60;
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

    public void setTextCenter(StatusbarIndex i, String text) {
        displayListForLeftLabel.put(i, text);
        setIndexForCenterDisplay(currentIndex);
    }

    /**
     * Wähle den Textstring aus dem Index {@code i}, der im linken Textfeld dargestellt werden soll.
     *
     * @param i
     */
    public void setIndexForCenterDisplay(StatusbarIndex i) {
        currentIndex = i;
        String displayString = displayListForLeftLabel.get(i);
        lblCenter.setText(displayString);
    }

    public enum StatusbarIndex {

        NONE, FILME, DOWNLOAD, ABO
    };

    //FIXME use swing timer
    private class AgeUpdateTimer extends Thread {

        private final int WARTEZEIT = 1000; // 1 Sekunde

        @Override
        public synchronized void run() {
            while (true) {
                try {
                    sleep(WARTEZEIT);
                    if (!stopTimer) {
                        SwingUtilities.invokeAndWait(new Runnable() {
                            @Override
                            public void run() {
                                setInfoRechts();
                            }
                        });
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(936251087, Log.FEHLER_ART_PROG, MVStatusBar.class.getName(), ex);
                }
            }
        }
    }
}
