package mediathek.gui;

import com.explodingpixels.macwidgets.BottomBar;
import com.explodingpixels.macwidgets.BottomBarSize;
import java.awt.FlowLayout;
import java.util.EnumMap;
import javax.swing.*;
import mSearch.filmeSuchen.MSListenerFilmeLaden;
import mSearch.filmeSuchen.MSListenerFilmeLadenEvent;
import mSearch.tool.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mSearch.tool.ListenerMediathekView;

/**
 * User: crystalpalace1977
 * Date: 02.03.13
 * Time: 19:54
 */
public final class MVStatusBar extends JPanel {

    private boolean stopTimer = false;
    private final EnumMap<MVStatusBar.StatusbarIndex, String> displayListForLeftLabel = new EnumMap<>(MVStatusBar.StatusbarIndex.class);
    private MVStatusBar.StatusbarIndex currentIndex = MVStatusBar.StatusbarIndex.NONE;

    private final JLabel lblLeft;
    private final JLabel lblRight;
    private final JProgressBar progress;
    private final JButton stopButton;
    private final BottomBar bottomBar;
    private final Daten daten;

    public MVStatusBar(Daten daten) {
        this.daten = daten;
        bottomBar = new BottomBar(BottomBarSize.LARGE);

        lblLeft = new JLabel();
        bottomBar.addComponentToLeft(lblLeft);

        if (Daten.debug) {
            bottomBar.addComponentToCenter(new MVMemoryUsageButton());
        }

        //Progress controls and Filminfo must be packed in a panel
        JPanel progressPanel = new JPanel();
        progressPanel.setBackground(bottomBar.getComponent().getBackground());
        progressPanel.setLayout(new FlowLayout());
        progressPanel.setOpaque(false);

        lblRight = new JLabel();
        progressPanel.add(lblRight);

        progress = new JProgressBar();
        progressPanel.add(progress);

        stopButton = new JButton();
        stopButton.setIcon(GetIcon.getProgramIcon("close.png"));

        stopButton.setToolTipText("Abbrechen");
        stopButton.addActionListener(e -> Daten.filmeLaden.setStop(true));

        progressPanel.add(stopButton);
        bottomBar.addComponentToRight(progressPanel);

        hideProgressIndicators();
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_TIMER, MVStatusBar.class.getSimpleName()) {
            @Override
            public void ping() {
                setTextForLeftDisplay();
                try {
                    if (!stopTimer) {
                        setTextForRightDisplay();
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(936251087, ex);
                }
            }
        });
        Daten.filmeLaden.addAdListener(new MSListenerFilmeLaden() {
            @Override
            public void start(MSListenerFilmeLadenEvent event) {
            }

            @Override
            public void progress(MSListenerFilmeLadenEvent event) {
                updateProgressBar(event);
            }

            @Override
            public void fertig(MSListenerFilmeLadenEvent event) {
                hideProgressIndicators();
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
    private void hideProgressIndicators() {
        stopTimer = false;
        progress.setVisible(false);
        stopButton.setVisible(false);
        lblRight.setVisible(true);
        setTextForRightDisplay();
    }

    private void updateProgressBar(MSListenerFilmeLadenEvent event) {
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
            lblRight.setText(GuiFunktionen.textLaenge(60, event.senderUrl, true /* mitte */, true /*addVorne*/));
        } else {
            lblRight.setVisible(false);
        }
    }

    /**
     * Wähle den Textstring aus dem Index {@code i}, der im zentralen Textfeld dargestellt werden soll.
     *
     * @param i Index, für den ein Text dargestellt werden soll.
     */
    public void setIndexForLeftDisplay(StatusbarIndex i) {
        currentIndex = i;
        String displayString = displayListForLeftLabel.get(i);
        lblLeft.setText(displayString);
    }

    public void setTextForLeftDisplay() {
        switch (currentIndex) {
            case FILME:
                setInfoFilme(); // muss laufen, wegen der Anzeige der Bandbreite
                break;
            case DOWNLOAD:
                setInfoDownload(); // muss laufen, wegen der Anzeige der Bandbreite
                break;
            case ABO:
                setInfoAbo();
                break;
            default:
        }
        String displayString = displayListForLeftLabel.get(currentIndex);
        lblLeft.setText(displayString);
    }

    private void setInfoFilme() {
        String textLinks;
        final String TRENNER = "  ||  ";
        int gesamt = Daten.listeFilme.size();
        int anzListe = daten.guiFilme.getTableRowCount();
        int runs = Daten.listeDownloadsButton.getListOfStartsNotFinished(DatenDownload.QUELLE_BUTTON).size();
        // Anzahl der Filme
        if (gesamt == anzListe) {
            if (anzListe == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = anzListe + " Filme";
            }
        } else {
            if (anzListe == 1) {
                textLinks = "1 Film";
            } else {
                textLinks = anzListe + " Filme";
            }
            textLinks += " (Insgesamt: " + gesamt + " )";
        }
        // laufende Programme
        if (runs == 1) {
            textLinks += TRENNER;
            textLinks += (runs + " laufender Film");
        } else if (runs > 1) {
            textLinks += TRENNER;
            textLinks += (runs + " laufende Filme");
        }
        // auch die Downloads anzeigen
        textLinks += TRENNER;
        textLinks += getInfoTextDownloads(false /*mitAbo*/);

        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.FILME, textLinks);
    }

    private void setInfoDownload() {
        String textLinks = getInfoTextDownloads(true /*mitAbo*/);

        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.DOWNLOAD, textLinks);
    }

    private void setInfoAbo() {

        String textLinks;
        int ein = 0;
        int aus = 0;
        int gesamt = Daten.listeAbo.size();
        for (DatenAbo abo : Daten.listeAbo) {
            if (abo.aboIstEingeschaltet()) {
                ++ein;
            } else {
                ++aus;
            }
        }
        if (gesamt == 1) {
            textLinks = "1 Abo, ";
        } else {
            textLinks = gesamt + " Abos, ";
        }
        textLinks += "(" + ein + " eingeschaltet, " + aus + " ausgeschaltet)";

        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.ABO, textLinks);
    }

    private String getInfoTextDownloads(boolean mitAbo) {
        String textLinks;
        // Text links: Zeilen Tabelle
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = Daten.downloadInfos.downloadStarts;
        if (starts[0] == 1) {
            textLinks = "1 Download";
        } else {
            textLinks = starts[0] + " Downloads";
        }
        if (mitAbo) {
            if (starts[1] == 1) {
                textLinks += " (1 Abo, ";
            } else {
                textLinks += " (" + starts[1] + " Abos, ";
            }
            if (starts[2] == 1) {
                textLinks += "1 Download)";
            } else {
                textLinks += starts[2] + " Downloads)";
            }
        }
        boolean print = false;
        for (int ii = 1; ii < starts.length; ++ii) {
            if (starts[ii] > 0) {
                print = true;
                break;
            }
        }
        if (print) {
            textLinks += ": ";
            if (starts[4] == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += starts[4] + " laufen";
            }

            if (starts[4] > 0) {
                textLinks += " (" + Daten.downloadInfos.bandwidthStr + ")";
            }

            if (starts[3] == 1) {
                textLinks += ", 1 wartet";
            } else {
                textLinks += ", " + starts[3] + " warten";
            }
            if (starts[5] > 0) {
                if (starts[5] == 1) {
                    textLinks += ", 1 fertig";
                } else {
                    textLinks += ", " + starts[5] + " fertig";
                }
            }
            if (starts[6] > 0) {
                if (starts[6] == 1) {
                    textLinks += ", 1 fehlerhaft";
                } else {
                    textLinks += ", " + starts[6] + " fehlerhaft";
                }
            }
        }
        return textLinks;
    }

    private void setTextForRightDisplay() {
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
        lblRight.setText(strText);
    }

}
