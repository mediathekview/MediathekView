package mediathek;

import com.explodingpixels.macwidgets.BottomBar;
import com.explodingpixels.macwidgets.BottomBarSize;
import com.explodingpixels.macwidgets.MacWidgetFactory;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.EnumMap;
import javax.swing.*;

import mediathek.daten.Daten;
import mediathek.tool.ListenerMediathekView;
import mediathek.controller.Log;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;

/**
 * User: crystalpalace1977
 * Date: 02.03.13
 * Time: 19:54
 */
public final class MVStatusBar extends JPanel {

    boolean stopTimer = false;
    EnumMap<MVStatusBar.StatusbarIndex, String> displayListForLeftLabel = new EnumMap<>(MVStatusBar.StatusbarIndex.class);
    MVStatusBar.StatusbarIndex currentIndex = MVStatusBar.StatusbarIndex.NONE;

    private JLabel lblCenter;
    private JLabel lblRechts;
    private JProgressBar progress;
    private JButton stopButton;
    private BottomBar bottomBar;

    public MVStatusBar() {
        bottomBar = new BottomBar(BottomBarSize.LARGE);

        lblCenter = MacWidgetFactory.createEmphasizedLabel("");
        bottomBar.addComponentToLeft(lblCenter);

        if (Daten.debug) {
            bottomBar.addComponentToCenter(new MVMemoryUsageButton());
        }

        //Progress controls and Filminfo must be packed in a panel
        JPanel progressPanel = new JPanel();
        progressPanel.setBackground(bottomBar.getComponent().getBackground());
        progressPanel.setLayout(new FlowLayout());
        progressPanel.setOpaque(false);

        lblRechts = MacWidgetFactory.createEmphasizedLabel("");
        progressPanel.add(lblRechts);

        progress = new JProgressBar();
        progressPanel.add(progress);

        stopButton = new JButton();
        //stopButton.setIcon(new ImageIcon(getClass().getResource("/com/explodingpixels/macwidgets/images/close.png")));
        stopButton.setIcon(GetIcon.getIcon("close.png"));

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
                    Log.fehlerMeldung(936251087, Log.FEHLER_ART_PROG, MVStatusBar.class.getName(), ex);
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
        strText += Daten.listeFilme.genDate();
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

    public void setTextLeft(StatusbarIndex i, String text) {
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
}
