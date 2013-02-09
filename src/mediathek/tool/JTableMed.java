/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.tool;

import java.util.List;
import javax.swing.JTable;
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
    public static final int TABELLE_STANDARD = -1;
    public static final int TABELLE_TAB_FILME = 0;
    public static final int TABELLE_TAB_DOWNLOADS = 1;
    public static final int TABELLE_TAB_ABOS = 2;
    public static final String FELDTRENNER = "|";
    private List<? extends javax.swing.RowSorter.SortKey> listeSortKeys = null;
    int[] breite;
    int[] reihe;
    private int indexSpalte = 0;
    private int sel = -1;
    private int[] selection;
    private String[] indexWertSelection = null;
    private String indexWertSel = null;
    private boolean stopBeob = false;
    //
    String[] spaltenTabelle;
    int nrDatenSystem;
    int tabelle;

    public JTableMed(int ttabelle) {
        tabelle = ttabelle;
        switch (tabelle) {
            case TABELLE_TAB_FILME:
                indexSpalte = 0; // Filmnummer
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME_NR;
                spaltenTabelle = DatenFilm.FILME_COLUMN_NAMES;
                this.setModel(new TModelFilm(new Object[][]{}, spaltenTabelle));
                break;
            case TABELLE_TAB_DOWNLOADS:
                indexSpalte = 1; // Filmnummer
                nrDatenSystem = Konstanten.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS_NR;
                spaltenTabelle = DatenDownload.DOWNLOAD_COLUMN_NAMES;
                this.setModel(new TModelDownload(new Object[][]{}, spaltenTabelle));
                break;
            case TABELLE_TAB_ABOS:
                indexSpalte = 0; // Abonummer
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

    public JTableMed(String[] sspaltenTabelle) {
        tabelle = TABELLE_STANDARD;
        spaltenTabelle = sspaltenTabelle;
        this.setModel(new TModel(new Object[][]{}, spaltenTabelle));
        breite = getArray(spaltenTabelle.length);
        reihe = getArray(spaltenTabelle.length);
        this.setAutoCreateRowSorter(true);
        this.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
    }

    public void fireTableDataChanged(boolean setSpalten) {
        if (setSpalten) {
            getSelected();
        }
        ((TModel) this.getModel()).fireTableDataChanged();
        if (setSpalten) {
            setSelected();
        }
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
//                if (!indexWertSel.equals("")) {
//                    int r = ((TModel) this.getModel()).getIdxRow(indexSpalte, indexWertSel);
//                    if (r >= 0) {
//                        // ansonsten gibts die Zeile nicht mehr
//                        r = this.convertRowIndexToView(r);
//                        this.setRowSelectionInterval(r, r);
//                    }
//                }
                if (indexWertSelection != null) {
                    for (String idx : indexWertSelection) {
                        int r = ((TModel) this.getModel()).getIdxRow(indexSpalte, idx);
                        if (r >= 0) {
                            // ansonsten gibts die Zeile nicht mehr
                            r = this.convertRowIndexToView(r);
                            this.addRowSelectionInterval(r, r);
                        }
                    }
                }
                indexWertSelection = null;
                break;
            case TABELLE_STANDARD:
//                if (sel >= 0 && sel < this.getRowCount()) {
//                    this.setRowSelectionInterval(sel, sel);
//                }
                if (selection != null) {
                    if (selection.length > 0) {
                        for (int i = 0; i < selection.length; ++i) {
                            if (selection[i] < this.getRowCount()) {
                                this.addRowSelectionInterval(selection[i], selection[i]);
                            }
                        }
                    }
                }
                break;
        }
        stopBeob = false;
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
            this.validate();
        } catch (Exception ex) {
            Log.fehlerMeldung(965001463, Log.FEHLER_ART_PROG, "JTableMed.setSpalten", ex);
        }
        stopBeob = false;
    }

    public void initTabelle() {
        // Tabelle das erste Mal initialisieren,
        // mit den gespeicherten Daten oder
        // mit den Standardwerten
        // erst die Breite, dann die Reihenfolge
        if (tabelle == TABELLE_STANDARD) {
            // wird nur für eingerichtete Tabellen gemacht
            return;
        }
        String b, r;
        boolean ok = false;
        if (!DDaten.system[nrDatenSystem].equals("")) {
            ok = true;
            //String d = DDaten.system[nrDatenSystem];
            b = DDaten.system[nrDatenSystem].substring(0, DDaten.system[nrDatenSystem].indexOf(FELDTRENNER));
            r = DDaten.system[nrDatenSystem].substring(DDaten.system[nrDatenSystem].indexOf(FELDTRENNER) + 1);
            if (!arrLesen(b, breite)) {
                ok = false;
            }
            if (!arrLesen(r, reihe)) {
                ok = false;
            }
        }
        if (ok) {
            setSpalten();
        } else {
            resetTabelle();
            // setSpalten wird im resetTabelle gemacht
        }
        // und jetzt erst der Beobachter, damit Daten.system nicht vorher schon überschrieben wird
        ///this.getColumnModel().addColumnModelListener(new BeobSpalten());
    }

    public void resetTabelle() {
        // Standardwerte wetzen
        for (int i = 0; i < spaltenTabelle.length; ++i) {
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
                            || i == DatenFilm.FILM_SENDER_NR) {
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
                        breite[i] = 300;
                    } else if (i == DatenDownload.DOWNLOAD_DATUM_NR
                            || i == DatenDownload.DOWNLOAD_ZEIT_NR
                            || i == DatenDownload.DOWNLOAD_SENDER_NR
                            || i == DatenDownload.DOWNLOAD_PROGRESS_NR) {
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
                case TABELLE_STANDARD:
                    break;
            }
        }
        this.setRowSorter(null);
        this.setAutoCreateRowSorter(true);
        setSpalten();
    }

    public void spaltenAusschalten() {
        for (int i = 0; i < spaltenTabelle.length; ++i) {
            switch (tabelle) {
                case TABELLE_TAB_FILME:
                    if (i == DatenFilm.FILM_URL_RTMP_NR
                            || i == DatenFilm.FILM_URL_AUTH_NR
                            || i == DatenFilm.FILM_URL_THEMA_NR) {
                        breite[i] = 0;
                    }
                    break;
                case TABELLE_TAB_DOWNLOADS:
                    if (i == DatenDownload.DOWNLOAD_URL_AUTH_NR
                            || i == DatenDownload.DOWNLOAD_URL_RTMP_NR
                            || i == DatenDownload.DOWNLOAD_ART_NR
                            || i == DatenDownload.DOWNLOAD_QUELLE_NR
                            || i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR
                            || i == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                        breite[i] = 0;
                    }
                    break;
                case TABELLE_TAB_ABOS:
                    break;
                case TABELLE_STANDARD:
                    break;
            }
            if (DDaten.debug) {
                // dann wird nichts ausgeschaltet
                if (breite[i] == 0) {
                    breite[i] = 100;
                }
            }
        }
    }

    public void tabelleNachDatenSchreiben() {
        if (tabelle == TABELLE_STANDARD) {
            // wird nur für eingerichtet Tabellen gemacht
            return;
        }
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

//    private class BeobSpalten implements TableColumnModelListener {
//        
//        @Override
//        public void columnAdded(TableColumnModelEvent arg0) {
//        }
//        
//        @Override
//        public void columnRemoved(TableColumnModelEvent arg0) {
//        }
//        
//        @Override
//        public void columnMoved(TableColumnModelEvent arg0) {
//            set();
//        }
//        
//        @Override
//        public void columnMarginChanged(ChangeEvent arg0) {
//            set();
//        }
//        
//        @Override
//        public void columnSelectionChanged(ListSelectionEvent arg0) {
//        }
//        
//        private void set() {
//            if (!stopBeob) {
///                tabelleNachDatenSchreiben();
//            }
//        }
//    }
    private int[] getArray(int anzahl) {
        int[] arr = new int[anzahl];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = -1;
        }
        return arr;
    }

    private boolean arrLesen(String s, int[] arr) {
        String sub;
        if (spaltenTabelle.length != countString(s) + 1) {
            // dann hat sich die Anzahl der Spalten der Tabelle geändert: Versionswechsel
            return false;
        } else {
            for (int i = 0; i < spaltenTabelle.length; i++) {
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

    private int countString(String s) {
        int ret = 0;
        for (int i = 0; i < s.length(); ++i) {
            if (s.charAt(i) == ',') {
                ++ret;
            }
        }
        return ret;
    }
}
