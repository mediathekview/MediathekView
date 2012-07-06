/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.tool;

import java.util.List;
import javax.swing.JTable;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;

/**
 *
 * @author emil
 */
public final class JTableMed extends JTable {

    public static final String TABELLEN = "Tabellen";
    public static final int TABELLE_EIGENSCHAFTEN_MAX = 2; // Breite, Reihenfolge
    public static final int TABELLEN_MAX = 3; // GuiFilme, GuiDownlaod, GuiAbo
    public static final int TABELLE_TAB_FILME = 0;
    public static final int TABELLE_TAB_DOWNLOADS = 1;
    public static final int TABELLE_TAB_ABOS = 2;
    public static final String FELDTRENNER = "|";
    private List<? extends javax.swing.RowSorter.SortKey> listeSortKeys = null;
    int[] breite;
    int[] reihe;
    private int sel;
    private int rows;
    private boolean stopBeob = false;
    //
    String[] spaltenTabelle;
    int nrDatenSystem;
    int tabelle;

    public JTableMed(int ttabelle) {
        tabelle = ttabelle;
        switch (tabelle) {
            case TABELLE_TAB_FILME:
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME_NR;
                spaltenTabelle = DatenFilm.FILME_COLUMN_NAMES;
                this.setModel(new TModelFilm(new Object[][]{}, spaltenTabelle));
                break;
            case TABELLE_TAB_DOWNLOADS:
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS_NR;
                spaltenTabelle = DatenDownload.DOWNLOAD_COLUMN_NAMES;
                this.setModel(new TModelDownload(new Object[][]{}, spaltenTabelle));
                break;
            case TABELLE_TAB_ABOS:
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS_NR;
                spaltenTabelle = DatenAbo.ABO_COLUMN_NAMES;
                this.setModel(new TModelAbo(new Object[][]{}, spaltenTabelle));
                break;
        }
        breite = getArray(spaltenTabelle.length);
        reihe = getArray(spaltenTabelle.length);
        this.setAutoCreateRowSorter(true);
        this.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
    }

    public void getSpalten() {
        // Einstellungen der Tabelle merken
        sel = this.getSelectedRow();
        rows = this.getRowCount();
        for (int i = 0; i < reihe.length && i < this.getModel().getColumnCount(); ++i) {
            reihe[i] = this.convertColumnIndexToModel(i);
        }
        for (int i = 0; i < breite.length && i < this.getModel().getColumnCount(); ++i) {
            breite[i] = getColumnModel().getColumn(
                    this.convertColumnIndexToView(i)).getWidth();
        }
        if (this.getRowSorter() != null) {
            listeSortKeys = this.getRowSorter().getSortKeys();
        }
    }

    /**
     *
     */
    public void setSpalten() {
        // gemerkte Einstellungen der Tabelle wieder setzten
        stopBeob = true;
        try {
            for (int i = 0; i < breite.length && i < this.getColumnCount(); ++i) {
                if (breite[i] == 0) {
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMinWidth(0);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setPreferredWidth(0);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMaxWidth(0);
                } else {
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMinWidth(10);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setPreferredWidth(breite[i]);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMaxWidth(3000);
                }
            }
            for (int i = 0; i < reihe.length && i < this.getColumnCount(); ++i) {
                this.getColumnModel().moveColumn(this.convertColumnIndexToView(reihe[i]), i);
            }
            if (listeSortKeys != null) {
                if (!listeSortKeys.isEmpty()) {
                    this.getRowSorter().setSortKeys(listeSortKeys);
                }
            }
            if (rows == getRowCount()) {
                if (sel >= 0 && sel < this.getRowCount()) {
                    this.setRowSelectionInterval(sel, sel);
                    this.scrollRectToVisible(getCellRect(sel, 0, false));
                }
            }
            this.validate();
        } catch (Exception ex) {
            Log.fehlerMeldung(965001463, "JTableMed.setSpalten", ex);
        }
        stopBeob = false;
    }

    public void initTabelle() {
        // Tabelle das erste Mal initialisieren,
        // mit den gespeicherten Daten oder
        // mit den Standardwerten
        // erst die Breite, dann die Reihenfolge
        String b, r;
        if (!DDaten.system[nrDatenSystem].equals("")) {
            String d = DDaten.system[nrDatenSystem];
            b = DDaten.system[nrDatenSystem].substring(0, DDaten.system[nrDatenSystem].indexOf(FELDTRENNER));
            r = DDaten.system[nrDatenSystem].substring(DDaten.system[nrDatenSystem].indexOf(FELDTRENNER) + 1);
            arrLesen(b, breite);
            arrLesen(r, reihe);
            setSpalten();
        } else {
            resetTabelle();
            // setSpalten wird im resetTabelle gemacht
        }
        // und jetzt erst der Beobachter, damit Daten.system nich vorher schon überschrieben wird
        this.getColumnModel().addColumnModelListener(new BeobSpalten());
    }

    public void resetTabelle() {
        // Standardwerte wetzen
        switch (tabelle) {
            case TABELLE_TAB_FILME:
                for (int i = 0; i < spaltenTabelle.length; ++i) {
                    reihe[i] = i;
                    breite[i] = 200;
                    if (i == DatenFilm.FILM_NR_NR) {
                        breite[i] = 75;
                    } else if (i == DatenFilm.FILM_TITEL_NR) {
                        breite[i] = 300;
                    } else if (i == DatenFilm.FILM_DATUM_NR
                            || i == DatenFilm.FILM_ZEIT_NR
                            || i == DatenFilm.FILM_SENDER_NR) {
                        breite[i] = 100;
                    } else if (i == DatenFilm.FILM_URL_NR) {
                        breite[i] = 500;
                    } else if (!DDaten.debug) {
                        if (i == DatenFilm.FILM_URL_ORG_NR
                                || i == DatenFilm.FILM_URL_RTMP_NR
                                || i == DatenFilm.FILM_URL_AUTH_NR
                                || i == DatenFilm.FILM_URL_THEMA_NR) {
                            breite[i] = 0;
                        }
                    }
                }
                break;
            case TABELLE_TAB_DOWNLOADS:
                for (int i = 0; i < spaltenTabelle.length; ++i) {
                    reihe[i] = i;
                    breite[i] = 200;
                    if (i == DatenDownload.DOWNLOAD_NR_NR
                            || i == DatenDownload.DOWNLOAD_ABO_NR
                            || i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                        breite[i] = 75;
                    } else if (i == DatenDownload.DOWNLOAD_TITEL_NR) {
                        breite[i] = 300;
                    } else if (i == DatenDownload.DOWNLOAD_DATUM_NR
                            || i == DatenDownload.DOWNLOAD_ZEIT_NR
                            || i == DatenDownload.DOWNLOAD_SENDER_NR) {
                        breite[i] = 100;
                    } else if (!DDaten.debug) {
                        if (i == DatenDownload.DOWNLOAD_URL_AUTH_NR
                                || i == DatenDownload.DOWNLOAD_URL_RTMP_NR
                                || i == DatenDownload.DOWNLOAD_ART_NR
                                || i == DatenDownload.DOWNLOAD_QUELLE_NR) {
                            breite[i] = 0;
                        }
                    }
                }
                break;
            case TABELLE_TAB_ABOS:
                for (int i = 0; i < spaltenTabelle.length; ++i) {
                    reihe[i] = i;
                    breite[i] = 200;
                    if (i == DatenAbo.ABO_NR_NR
                            || i == DatenAbo.ABO_EINGESCHALTET_NR) {
                        breite[i] = 75;
                    } else if (i == DatenAbo.ABO_DOWN_DATUM_NR
                            || i == DatenAbo.ABO_SENDER_NR) {
                        breite[i] = 100;
                    }
                }
                break;
        }
        setSpalten();
    }

    private void tabelleNachDatenSchreiben() {
        // Tabellendaten ind die Daten.system schreiben
        // erst die Breite, dann die Reihenfolge
        String b, r;
        int reihe_[] = new int[spaltenTabelle.length];
        int breite_[] = new int[spaltenTabelle.length];
        for (int i = 0; i < reihe_.length && i < this.getModel().getColumnCount(); ++i) {
            reihe_[i] = this.convertColumnIndexToModel(i);
        }
        for (int i = 0; i < breite_.length && i < this.getModel().getColumnCount(); ++i) {
            breite_[i] = this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).getWidth();
        }
        b = Integer.toString(breite_[0]);
        r = Integer.toString(reihe_[0]);
        for (int i = 1; i < breite.length; i++) {
            b = b + "," + Integer.toString(breite_[i]);
            r = r + "," + Integer.toString(reihe_[i]);
        }
        DDaten.system[nrDatenSystem] = b + FELDTRENNER + r;
    }

    private class BeobSpalten implements TableColumnModelListener {

        @Override
        public void columnAdded(TableColumnModelEvent arg0) {
        }

        @Override
        public void columnRemoved(TableColumnModelEvent arg0) {
        }

        @Override
        public void columnMoved(TableColumnModelEvent arg0) {
            set();
        }

        @Override
        public void columnMarginChanged(ChangeEvent arg0) {
            set();
        }

        @Override
        public void columnSelectionChanged(ListSelectionEvent arg0) {
        }

        private void set() {
            if (!stopBeob) {
                tabelleNachDatenSchreiben();
            }
        }
    }

    private int[] getArray(int anzahl) {
        int[] arr = new int[anzahl];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = -1;
        }
        return arr;
    }

    private void arrLesen(String s, int[] arr) {
        String sub = "";
        for (int i = 0; i < spaltenTabelle.length; i++) {
            if (!s.equals("")) {
                if (s.contains(",")) {
                    sub = s.substring(0, s.indexOf(","));
                    s = s.replaceFirst(sub + ",", "");
                } else {
                    sub = s;
                    s = "";
                }
                arr[i] = Integer.parseInt(sub);
            }
        }
    }
}
