package mediathek.gui;

import com.explodingpixels.macwidgets.BottomBar;
import com.explodingpixels.macwidgets.BottomBarSize;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Functions;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.SoftBevelBorder;
import java.awt.*;
import java.util.EnumMap;

@SuppressWarnings("serial")
public final class MVStatusBar extends JPanel {
    private final Daten daten;
    private boolean stopTimer = false;
    private final EnumMap<MVStatusBar.StatusbarIndex, String> displayListForLeftLabel = new EnumMap<>(MVStatusBar.StatusbarIndex.class);
    private MVStatusBar.StatusbarIndex currentIndex = MVStatusBar.StatusbarIndex.NONE;

    private final JLabel lblLeft;
    private final JLabel lblRight;
    private final JLabel lblSel;
    private final JProgressBar progress;
    private final JButton stopButton;
    private final BottomBar bottomBar;
    private final String TRENNER = "  ||  ";

    public MVStatusBar() {
        daten = Daten.getInstance();
                EmptyBorder eBorder = new EmptyBorder(0, 5, 0, 5); // oben, rechts, unten, links
        bottomBar = new BottomBar(BottomBarSize.LARGE);
        SoftBevelBorder sbb = new SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED);

        lblSel = new JLabel("0");
        lblSel.setBorder(BorderFactory.createCompoundBorder(sbb, eBorder));
        bottomBar.addComponentToLeft(lblSel, 20);

        lblLeft = new JLabel();
        bottomBar.addComponentToLeft(lblLeft);

        if (Daten.isDebug()) {
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
        stopButton.setIcon(Icons.ICON_STATUSBAR_STOP);
        stopButton.setToolTipText("Abbrechen");
        stopButton.addActionListener(e -> daten.getFilmeLaden().setStop(true));

        progressPanel.add(stopButton);
        bottomBar.addComponentToRight(progressPanel);

        hideProgressIndicators();
        Listener.addListener(new Listener(Listener.EREIGNIS_TIMER, MVStatusBar.class.getSimpleName()) {
            @Override
            public void ping() {
                setTextForLeftDisplay();
                try {
                    if (!stopTimer) {
                        setTextForRightDisplay();
                    }
                } catch (Exception ex) {
                    Log.errorLog(936251087, ex);
                }
            }
        });
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
                updateProgressBar(event);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
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

    private void updateProgressBar(ListenerFilmeLadenEvent event) {
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
        if (Daten.isDebug()) {
            lblRight.setText(Functions.textLaenge(60, event.senderUrl, true /* mitte */, true /*addVorne*/));
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
                setInfoDefault();
        }
        String displayString = displayListForLeftLabel.get(currentIndex);
        lblLeft.setText(displayString);
    }

    private void setInfoDefault() {
        String textLinks;
        int gesamt = daten.getListeFilme().size();
        lblSel.setText("");
        // Anzahl der Filme
        if (gesamt == 1) {
            textLinks = "1 Film";
        } else {
            textLinks = gesamt + " Filme";
        }
        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.NONE, textLinks);
    }

    private void setInfoFilme() {
        String textLinks;
        int gesamt = daten.getListeFilme().size();
        int anzListe = Daten.guiFilme.getTableRowCount();
        int runs = daten.getListeDownloadsButton().getListOfStartsNotFinished(DatenDownload.QUELLE_BUTTON).size();
        lblSel.setText(Daten.guiFilme.tabelle.getSelectedRowCount() + "");
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
        textLinks += getInfoTextDownloads(false);

        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.FILME, textLinks);
    }

    private void setInfoDownload() {
        lblSel.setText(Daten.guiDownloads.tabelle.getSelectedRowCount() + "");
        String textLinks = getInfoTextDownloads(true /*mitAbo*/);

        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.DOWNLOAD, textLinks);
    }

    private void setInfoAbo() {
        lblSel.setText(Daten.guiAbo.tabelle.getSelectedRowCount() + "");

        String textLinks;
        int ein = 0;
        int aus = 0;
        int gesamt = daten.getListeAbo().size();
        for (DatenAbo abo : daten.getListeAbo()) {
            if (abo.aboIstEingeschaltet()) {
                ++ein;
            } else {
                ++aus;
            }
        }
        if (gesamt == 1) {
            textLinks = "1 Abo";
        } else {
            textLinks = gesamt + " Abos";
        }
        textLinks += TRENNER + ein + " eingeschaltet, " + aus + " ausgeschaltet";

        displayListForLeftLabel.put(MVStatusBar.StatusbarIndex.ABO, textLinks);
    }

    private String getInfoTextDownloads(boolean download) {
        String textLinks;
        // Text links: Zeilen Tabelle
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = daten.getDownloadInfos().downloadStarts;
        int anz = daten.getListeDownloads().size();
        int diff = anz - starts[0];

        boolean print = false;
        for (int ii = 1; ii < starts.length; ++ii) {
            if (starts[ii] > 0) {
                print = true;
                break;
            }
        }

        if (anz == 1) {
            textLinks = "1 Download";
        } else {
            textLinks = anz + " Downloads";
        }
        if (download) {
            if (diff == 1) {
                textLinks += " (1 zurückgestellt)";
            } else if (diff > 1) {
                textLinks += " (" + diff + " zurückgestellt)";
            }
            textLinks += TRENNER;
            if (starts[1] == 1) {
                textLinks += "1 Abo, ";
            } else {
                textLinks += "" + starts[1] + " Abos, ";
            }
            if (starts[2] == 1) {
                textLinks += "1 Download";
            } else {
                textLinks += starts[2] + " Downloads";
            }
            textLinks += TRENNER;
        } else if (print) {
            textLinks += ": ";
        }

        if (print) {
            if (starts[4] == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += starts[4] + " laufen";
            }

            if (starts[4] > 0) {
                textLinks += " (" + daten.getDownloadInfos().bandwidthStr + ")";
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
        strText += daten.getListeFilme().genDate();
        strText += " Uhr  ";

        final int sekunden = daten.getListeFilme().getAge();

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
