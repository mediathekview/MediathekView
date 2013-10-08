/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
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
package mediathek.tool;

import java.util.LinkedList;
import java.util.List;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import mediathek.daten.DDaten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import msearch.daten.DatenFilm;

/**
 *
 * @author emil
 */
public final class MVJTable extends JTable {

    public static final String TABELLEN = "Tabellen";
    public static final int TABELLE_STANDARD = -1;
    public static final int TABELLE_TAB_FILME = 0;
    public static final int TABELLE_TAB_DOWNLOADS = 1;
    public static final int TABELLE_TAB_ABOS = 2;
    public static final int TABELLE_TAB_PSET = 3;
    public static final int TABELLE_TAB_PROG = 4;
    public static final String FELDTRENNER = "|";
    public static final String SORT_ASCENDING = "ASCENDING";
    public static final String SORT_DESCENDING = "DESCENDING";
    private List<? extends RowSorter.SortKey> listeSortKeys = null;
    int[] breite;
    int[] reihe;
    private int indexSpalte = 0;
    private int sel = -1;
    private int[] selection;
    private String[] indexWertSelection = null;
    private boolean[] spaltenAnzeigen;
    private String indexWertSel = null;
    private boolean stopBeob = false;
    //
    int nrDatenSystem = 0;
    int tabelle;
    String[] spaltenTitel;
    int maxSpalten;

    public MVJTable(int ttabelle) {
        tabelle = ttabelle;
        this.setAutoCreateRowSorter(true);
        this.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        switch (tabelle) {
            case TABELLE_TAB_FILME:
                spaltenTitel = DatenFilm.COLUMN_NAMES;
                maxSpalten = DatenFilm.MAX_ELEM;
                spaltenAnzeigen = getSpaltenEinAus(DatenFilm.spaltenAnzeigen, DatenFilm.MAX_ELEM);
                indexSpalte = DatenFilm.FILM_NR_NR;
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME_NR;
                this.setModel(new TModelFilm(new Object[][]{}, spaltenTitel));
                break;
            case TABELLE_TAB_DOWNLOADS:
                spaltenTitel = DatenDownload.COLUMN_NAMES;
                maxSpalten = DatenDownload.MAX_ELEM;
                spaltenAnzeigen = getSpaltenEinAus(DatenDownload.spaltenAnzeigen, DatenDownload.MAX_ELEM);
                //indexSpalte = DatenDownload.DOWNLOAD_FILM_NR_NR;
                indexSpalte = DatenDownload.DOWNLOAD_URL_NR;
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS_NR;
                this.setModel(new TModelDownload(new Object[][]{}, spaltenTitel));
                break;
            case TABELLE_TAB_ABOS:
                spaltenTitel = DatenAbo.COLUMN_NAMES;
                maxSpalten = DatenAbo.MAX_ELEM;
                spaltenAnzeigen = getSpaltenEinAus(DatenAbo.spaltenAnzeigen, DatenAbo.MAX_ELEM);
                indexSpalte = DatenAbo.ABO_NR_NR;
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS_NR;
                this.setModel(new TModelAbo(new Object[][]{}, spaltenTitel));
                break;
            case TABELLE_TAB_PSET:
                spaltenTitel = DatenPset.COLUMN_NAMES;
                maxSpalten = DatenPset.MAX_ELEM;
                spaltenAnzeigen = getSpaltenEinAus(DatenPset.spaltenAnzeigen, DatenPset.MAX_ELEM);
                indexSpalte = 0;
                nrDatenSystem = -1;
                this.setModel(new TModel(new Object[][]{}, spaltenTitel));
                this.setRowSorter(null);
                this.setAutoCreateRowSorter(false); // Reihenfolge ist die Anzeige der Button!
                break;
            case TABELLE_TAB_PROG:
                spaltenTitel = DatenProg.COLUMN_NAMES;
                maxSpalten = DatenProg.MAX_ELEM;
                spaltenAnzeigen = getSpaltenEinAus(DatenProg.spaltenAnzeigen, DatenProg.MAX_ELEM);
                indexSpalte = 0;
                nrDatenSystem = -1;
                this.setModel(new TModel(new Object[][]{}, spaltenTitel));
                break;
        }
        breite = getArray(maxSpalten);
        reihe = getArray(maxSpalten);
    }

    public void initTabelle() {
        // Tabelle das erste Mal initialisieren,
        // mit den gespeicherten Daten oder
        // mit den Standardwerten
        // erst die Breite, dann die Reihenfolge
        try {
            if (nrDatenSystem == -1) {
                // wird nur für eingerichtete Tabellen gemacht
                return;
            }
            String b = "", r = "", s = "", upDown = "";
            boolean ok = false;
            if (!DDaten.system[nrDatenSystem].equals("")) {
                ok = true;
                int f1, f2, f3;
                String d = DDaten.system[nrDatenSystem];
                if ((f1 = DDaten.system[nrDatenSystem].indexOf(FELDTRENNER)) != -1) {
                    b = DDaten.system[nrDatenSystem].substring(0, f1);
                    if ((f2 = DDaten.system[nrDatenSystem].indexOf(FELDTRENNER, f1 + 1)) != -1) {
                        r = DDaten.system[nrDatenSystem].substring(f1 + 1, f2);
                    }
                    if ((f3 = DDaten.system[nrDatenSystem].indexOf(FELDTRENNER, f2 + 1)) != -1) {
                        s = DDaten.system[nrDatenSystem].substring(f2 + 1, f3);
                        upDown = DDaten.system[nrDatenSystem].substring(f3 + 1);
                    }
                }
                if (!arrLesen(b, breite)) {
                    ok = false;
                }
                if (!arrLesen(r, reihe)) {
                    ok = false;
                }
                SortKey sk = sortKeyLesen(s, upDown);
                if (sk != null) {
                    LinkedList<SortKey> listSortKeys_ = new LinkedList<>();
                    listSortKeys_.add(sk);
                    this.getRowSorter().setSortKeys(listSortKeys_);
                }
            }
            if (ok) {
                setSpaltenEinAus(breite, spaltenAnzeigen);
                setSpalten();
            } else {
                resetTabelle();
            }
        } catch (Exception ex) {
            //vorsichtshalber
        }
    }

    private boolean anzeigen(int i, boolean[] spaltenAnzeigen) {
        return spaltenAnzeigen[i];
    }

    private void setSpaltenEinAus(int[] nr, boolean[] spaltenAnzeigen) {
        for (int i = 0; i < spaltenAnzeigen.length; ++i) {
            spaltenAnzeigen[i] = nr[i] > 0;
        }
    }

    private boolean[] getSpaltenEinAus(boolean[] spaltenAnzeigen, int MAX_ELEM) {
//        spaltenAnzeigen = new boolean[MAX_ELEM];
        for (int i = 0; i < MAX_ELEM; ++i) {
            spaltenAnzeigen[i] = true;
        }
        return spaltenAnzeigen;
    }

    public void fireTableDataChanged(boolean setSpalten) {
//        this.selectionModel.setValueIsAdjusting(true);
        if (setSpalten) {
            getSelected();
        }
        ((TModel) this.getModel()).fireTableDataChanged();
        if (setSpalten) {
            setSelected();
        }
//        this.selectionModel.setValueIsAdjusting(false);
    }

    public void getSelected() {
        // Einstellungen der Tabelle merken
        sel = this.getSelectedRow();
        selection = this.getSelectedRows();
        if (sel >= 0) {
            indexWertSel = this.getModel().getValueAt(this.convertRowIndexToModel(sel), indexSpalte).toString();
        } else {
            indexWertSel = "";
        }
        if (selection != null) {
            if (selection.length > 0) {
                indexWertSelection = new String[selection.length];
                for (int i = 0; i < selection.length; ++i) {
                    indexWertSelection[i] = this.getModel().getValueAt(this.convertRowIndexToModel(selection[i]), indexSpalte).toString();
                }
            }
        }
    }

    public void setSelected() {
        // gemerkte Einstellungen der Tabelle wieder setzten
        stopBeob = true;
        switch (tabelle) {
            case TABELLE_TAB_FILME:
            case TABELLE_TAB_DOWNLOADS:
            case TABELLE_TAB_ABOS:
                if (indexWertSelection != null) {
                    this.selectionModel.setValueIsAdjusting(true);
                    for (String idx : indexWertSelection) {
                        int r = ((TModel) this.getModel()).getIdxRow(indexSpalte, idx);
                        if (r >= 0) {
                            // ansonsten gibts die Zeile nicht mehr
                            r = this.convertRowIndexToView(r);
                            this.addRowSelectionInterval(r, r);
                        }
                    }
                    this.selectionModel.setValueIsAdjusting(false);
                }
                indexWertSelection = null;
                break;
            case TABELLE_STANDARD:
                if (selection != null) {
                    if (selection.length > 0) {
                        this.selectionModel.setValueIsAdjusting(true);
                        for (int i = 0; i < selection.length; ++i) {
                            if (selection[i] < this.getRowCount()) {
                                this.addRowSelectionInterval(selection[i], selection[i]);
                            }
                        }
                        this.selectionModel.setValueIsAdjusting(false);
                    }
                }
                break;
        }
        stopBeob = false;
    }

    public void spaltenEinAus() {
        for (int i = 0; i < breite.length && i < this.getColumnCount(); ++i) {
            if (!anzeigen(i, spaltenAnzeigen)) {
                // geänderte Ansicht der Spalten abfragen
                breite[i] = 0;
            } else {
                if (breite[i] == 0) {
                    breite[i] = 100; // damit sie auch zu sehen ist :)
                }
            }
        }
        for (int i = 0; i < breite.length && i < this.getColumnCount(); ++i) {
            if (breite[i] == 0) {
                this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMinWidth(0);
                this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setPreferredWidth(0);
                this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMaxWidth(0);
            } else {
                this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMinWidth(10);
                this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMaxWidth(3000);
                this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setPreferredWidth(breite[i]);
            }
        }
//        this.validate();
    }

    public void getSpalten() {
        // Einstellungen der Tabelle merken
        getSelected();
        for (int i = 0; i < reihe.length && i < this.getModel().getColumnCount(); ++i) {
            reihe[i] = this.convertColumnIndexToModel(i);
        }
        for (int i = 0; i < breite.length && i < this.getModel().getColumnCount(); ++i) {
            breite[i] = getColumnModel().getColumn(
                    this.convertColumnIndexToView(i)).getWidth();
        }
        if (this.getRowSorter() != null) {
            listeSortKeys = this.getRowSorter().getSortKeys();
        } else {
            listeSortKeys = null;
        }
    }

    public void setSpalten() {
        // gemerkte Einstellungen der Tabelle wieder setzten
        stopBeob = true;
        spaltenAusschalten();
        setSelected();
        try {
            for (int i = 0; i < breite.length && i < this.getColumnCount(); ++i) {
                if (!anzeigen(i, spaltenAnzeigen)) {
                    // geänderte Ansicht der Spalten abfragen
                    breite[i] = 0;
                } else {
                    if (breite[i] == 0) {
                        breite[i] = 100; // damit sie auch zu sehen ist :)
                    }
                }
            }
            for (int i = 0; i < breite.length && i < this.getColumnCount(); ++i) {
                if (breite[i] == 0) {
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMinWidth(0);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setPreferredWidth(0);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMaxWidth(0);
                } else {
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMinWidth(10);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setMaxWidth(3000);
                    this.getColumnModel().getColumn(this.convertColumnIndexToView(i)).setPreferredWidth(breite[i]);
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
            this.validate();
        } catch (Exception ex) {
            Log.fehlerMeldung(965001463, Log.FEHLER_ART_PROG, "JTableMed.setSpalten", ex);
        }
        stopBeob = false;
    }

    public void resetTabelle() {
        // Standardwerte wetzen
        for (int i = 0; i < maxSpalten; ++i) {
            switch (tabelle) {
                case TABELLE_TAB_FILME:
                    reihe[i] = i;
                    breite[i] = 200;
                    if (i == DatenFilm.FILM_NR_NR) {
                        breite[i] = 75;
                    } else if (i == DatenFilm.FILM_TITEL_NR) {
                        breite[i] = 300;
                    } else if (i == DatenFilm.FILM_DATUM_NR
                            || i == DatenFilm.FILM_ZEIT_NR
                            || i == DatenFilm.FILM_SENDER_NR
                            || i == DatenFilm.FILM_GROESSE_NR
                            || i == DatenFilm.FILM_DAUER_NR) {
                        breite[i] = 100;
                    } else if (i == DatenFilm.FILM_URL_NR) {
                        breite[i] = 500;
                    }
                    break;
                case TABELLE_TAB_DOWNLOADS:
                    reihe[i] = i;
                    breite[i] = 200;
                    if (i == DatenDownload.DOWNLOAD_NR_NR
                            || i == DatenDownload.DOWNLOAD_FILM_NR_NR
                            || i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                        breite[i] = 75;
                    } else if (i == DatenDownload.DOWNLOAD_TITEL_NR) {
                        breite[i] = 250;
                    } else if (i == DatenDownload.DOWNLOAD_ABO_NR
                            || i == DatenDownload.DOWNLOAD_THEMA_NR) {
                        breite[i] = 150;
                    } else if (i == DatenDownload.DOWNLOAD_DATUM_NR
                            || i == DatenDownload.DOWNLOAD_ZEIT_NR
                            || i == DatenDownload.DOWNLOAD_GROESSE_NR
                            || i == DatenDownload.DOWNLOAD_SENDER_NR
                            || i == DatenDownload.DOWNLOAD_PROGRESS_NR
                            || i == DatenDownload.DOWNLOAD_RESTZEIT_NR
                            || i == DatenDownload.DOWNLOAD_DAUER_NR) {
                        breite[i] = 100;
                    }
                    break;
                case TABELLE_TAB_ABOS:
                    reihe[i] = i;
                    breite[i] = 200;
                    if (i == DatenAbo.ABO_NR_NR
                            || i == DatenAbo.ABO_EINGESCHALTET_NR) {
                        breite[i] = 75;
                    } else if (i == DatenAbo.ABO_DOWN_DATUM_NR
                            || i == DatenAbo.ABO_SENDER_NR) {
                        breite[i] = 100;
                    }
                    break;
                default:
                    break;
            }
        }
        listeSortKeys = null;
        this.getRowSorter().setSortKeys(null);
        this.setRowSorter(null);
        this.setAutoCreateRowSorter(true);
        spaltenAusschalten();
        setSpaltenEinAus(breite, spaltenAnzeigen);
        setSpalten();
    }

    private void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            switch (tabelle) {
                case TABELLE_TAB_FILME:
                    if (i == DatenFilm.FILM_BESCHREIBUNG_NR
                            || i == DatenFilm.FILM_KEYWORDS_NR
                            || i == DatenFilm.FILM_WEBSEITE_NR
                            || i == DatenFilm.FILM_IMAGE_URL_NR
                            || i == DatenFilm.FILM_URL_RTMP_NR
                            || i == DatenFilm.FILM_URL_AUTH_NR
                            || i == DatenFilm.FILM_URL_HD_NR
                            || i == DatenFilm.FILM_URL_RTMP_HD_NR
                            || i == DatenFilm.FILM_URL_KLEIN_NR
                            || i == DatenFilm.FILM_URL_RTMP_KLEIN_NR) {
                        breite[i] = 0;
                    }
                    break;
                case TABELLE_TAB_DOWNLOADS:
                    if (i == DatenDownload.DOWNLOAD_FILM_URL_NR
                            || i == DatenDownload.DOWNLOAD_URL_RTMP_NR
                            || i == DatenDownload.DOWNLOAD_URL_AUTH_NR
                            || i == DatenDownload.DOWNLOAD_PROGRAMM_NR
                            || i == DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR
                            || i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR
                            || i == DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR
                            || i == DatenDownload.DOWNLOAD_ZIEL_PFAD_NR
                            || i == DatenDownload.DOWNLOAD_ART_NR
                            || i == DatenDownload.DOWNLOAD_QUELLE_NR
                            || i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR) {
                        breite[i] = 0;
                    }
                    break;
                case TABELLE_TAB_ABOS:
                    break;
                case TABELLE_STANDARD:
                    break;
            }
//            if (DDaten.debug) {
//                // dann wird nichts ausgeschaltet
//                if (breite[i] == 0) {
//                    breite[i] = 100;
//                }
//            }
        }
    }

    public void tabelleNachDatenSchreiben() {
        if (tabelle == TABELLE_STANDARD) {
            // wird nur für eingerichtet Tabellen gemacht
            return;
        }
        // Tabellendaten ind die Daten.system schreiben
        // erst die Breite, dann die Reihenfolge
        String b, r, s = "", upDown = "";
        int reihe_[] = new int[maxSpalten];
        int breite_[] = new int[maxSpalten];
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
        listeSortKeys = this.getRowSorter().getSortKeys();
        if (listeSortKeys != null) {
            if (!listeSortKeys.isEmpty()) {
                SortKey sk = listeSortKeys.get(0);
                s = String.valueOf(sk.getColumn());
                upDown = sk.getSortOrder().equals(SortOrder.ASCENDING) ? SORT_ASCENDING : SORT_DESCENDING;
            }
        }
        DDaten.system[nrDatenSystem] = b + FELDTRENNER + r + FELDTRENNER + s + FELDTRENNER + upDown;
    }

    private int[] getArray(int anzahl) {
        int[] arr = new int[anzahl];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = -1;
        }
        return arr;
    }

    private boolean arrLesen(String s, int[] arr) {
        String sub;
        if (maxSpalten != countString(s)) {
            // dann hat sich die Anzahl der Spalten der Tabelle geändert: Versionswechsel
            return false;
        } else {
            for (int i = 0; i < maxSpalten; i++) {
                if (!s.equals("")) {
                    if (s.contains(",")) {
                        sub = s.substring(0, s.indexOf(","));
                        s = s.replaceFirst(sub + ",", "");
                    } else {
                        sub = s;
                        s = "";
                    }
                    try {
                        arr[i] = Integer.parseInt(sub);
                    } catch (Exception ex) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private SortKey sortKeyLesen(String s, String upDown) {
        SortKey sk;
        int sp;
        try {
            sp = Integer.parseInt(s);
            if (upDown.equals(SORT_DESCENDING)) {
                sk = new SortKey(sp, SortOrder.DESCENDING);
            } else {
                sk = new SortKey(sp, SortOrder.ASCENDING);
            }
        } catch (Exception ex) {
            return null;
        }
        return sk;
    }

    private int countString(String s) {
        int ret = 0;
        for (int i = 0; i < s.length(); ++i) {
            if (s.charAt(i) == ',') {
                ++ret;
            }
        }
        return ++ret;
    }
}
