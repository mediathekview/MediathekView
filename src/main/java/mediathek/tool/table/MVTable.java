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
package mediathek.tool.table;

import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.MVConfig;
import mediathek.tool.MVFont;
import mediathek.tool.TModel;

import javax.swing.*;
import javax.swing.RowSorter.SortKey;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("serial")
public abstract class MVTable extends JTable {
    private static final String FELDTRENNER = "|";
    private static final String SORT_ASCENDING = "ASCENDING";
    private static final String SORT_DESCENDING = "DESCENDING";
    final int[] breite;
    final int[] reihe;
    private boolean iconAnzeigen = false;
    public boolean iconKlein = false;
    public boolean lineBreak = true;
    int maxSpalten;
    private List<? extends RowSorter.SortKey> listeSortKeys = null;
    int indexSpalte = 0;
    int[] selRows;
    int[] selIndexes = null;
    int selRow = -1;
    protected boolean[] spaltenAnzeigen;
    MVConfig.Configs nrDatenSystem = null;
    MVConfig.Configs iconAnzeigenStr = null;
    MVConfig.Configs iconKleinStr = null;

    public boolean getShowIcons() {
        return iconAnzeigen;
    }

    public void setShowIcon(boolean newVal) {
        iconAnzeigen = newVal;
    }

    public MVTable() {
        setAutoCreateRowSorter(true);
        setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        setupTableType();

        getTableHeader().addMouseListener(new WidthAdjuster(this));


        breite = getArray(maxSpalten);
        reihe = getArray(maxSpalten);

        if (iconAnzeigenStr != null) {
            iconAnzeigen = Boolean.parseBoolean(MVConfig.get(iconAnzeigenStr));
        }
        if (iconKleinStr != null) {
            iconKlein = Boolean.parseBoolean(MVConfig.get(iconKleinStr));
        }

        setHeight();
        Listener.addListener(new Listener(Listener.EREIGNIS_FONT, MVTable.class.getSimpleName()) {
            @Override
            public void ping() {
                setHeight();
            }
        });
    }

    private static SortKey sortKeyLesen(String s, String upDown) {
        SortKey sk;

        try {
            final int sp = Integer.parseInt(s);
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

    /**
     * Setup table specific stuff here.
     */
    protected abstract void setupTableType();

    public void setLineBreak(boolean lb) {
        lineBreak = lb;
    }

    public void invertSelection() {
        final ListSelectionModel mdl = getSelectionModel();
        final int[] selected = getSelectedRows();
        mdl.setValueIsAdjusting(true);
        mdl.setSelectionInterval(0, getRowCount() - 1);
        for (int i : selected) {
            mdl.removeSelectionInterval(i, i);
        }
        mdl.setValueIsAdjusting(false);
    }

    /**
     * Helper function for setHeight().
     *
     * @return sizeArea
     */
    protected int getSizeArea() {
        return 0;
    }

    public void setHeight() {
        final int sizeArea = getSizeArea();

        int size;
        if (!iconAnzeigen) {
            if (MVFont.fontSize < 15) {
                size = 18;
            } else {
                size = MVFont.fontSize + MVFont.fontSize / 3;
            }
        } else if (iconKlein) {
            if (MVFont.fontSize < 18) {
                size = 20;
            } else {
                size = MVFont.fontSize + MVFont.fontSize / 3;
            }
        } else if (MVFont.fontSize < 30) {
            size = 36;
        } else {
            size = MVFont.fontSize + MVFont.fontSize / 3;
        }

        setRowHeight(size > sizeArea ? size : sizeArea);
    }

    public void initTabelle() {
        // Tabelle das erste Mal initialisieren,
        // mit den gespeicherten Daten oder
        // den Standardwerten
        // erst die Breite, dann die Reihenfolge
        try {
            if (nrDatenSystem == null) {
                // wird nur für eingerichtete Tabellen gemacht
                return;
            }
            String b = "", r = "", s = "", upDown = "";
            boolean ok = false;
            if (!MVConfig.get(nrDatenSystem).isEmpty()) {
                ok = true;
                int f1, f2, f3;
                //String d = Daten.system[nrDatenSystem];
                final String strNrDatenSystem = MVConfig.get(nrDatenSystem);

                if ((f1 = strNrDatenSystem.indexOf(FELDTRENNER)) != -1) {
                    b = strNrDatenSystem.substring(0, f1);
                    if ((f2 = strNrDatenSystem.indexOf(FELDTRENNER, f1 + 1)) != -1) {
                        r = strNrDatenSystem.substring(f1 + 1, f2);
                    }
                    if ((f3 = strNrDatenSystem.indexOf(FELDTRENNER, f2 + 1)) != -1) {
                        s = strNrDatenSystem.substring(f2 + 1, f3);
                        upDown = strNrDatenSystem.substring(f3 + 1);
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
                    final ArrayList<SortKey> listSortKeys_ = new ArrayList<>();
                    listSortKeys_.add(sk);
                    this.getRowSorter().setSortKeys(listSortKeys_);
                }
            }
            if (ok) {
                setSpaltenEinAus(breite, spaltenAnzeigen);
                setSpalten();
                setHeight();
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

    boolean[] getSpaltenEinAus(boolean[] spaltenAnzeigen, int MAX_ELEM) {
        for (int i = 0; i < MAX_ELEM; ++i) {
            spaltenAnzeigen[i] = true;
        }
        return spaltenAnzeigen;
    }

    public void fireTableDataChanged(boolean setSpalten) {
        if (setSpalten) {
            getSelected();
        }
        ((TModel) getModel()).fireTableDataChanged();
        if (setSpalten) {
            setSelected();
        }
    }

    public void requestFocusSelect(JScrollPane jScrollPane) {
        requestFocus();
        if (getRowCount() > 0) {
            // sonst ist schon eine Zeile markiert
            if (getSelectedRow() == -1) {
                setRowSelectionInterval(0, 0);
            }
            final int firstSelectedRow = getSelectedRow();
            Rectangle cellLocation = getCellRect(firstSelectedRow, 0, false);
            jScrollPane.getVerticalScrollBar().setValue(cellLocation.y);
        }
    }

    public void requestFocusSelect(JScrollPane jScrollPane, int zeile) {
        requestFocus();
        if (getRowCount() > 0) {
            // sonst ist schon eine Zeile markiert
            setRowSelectionInterval(zeile, zeile);
            final int firstSelectedRow = getSelectedRow();
            Rectangle cellLocation = getCellRect(firstSelectedRow, 0, false);
            jScrollPane.getVerticalScrollBar().setValue(cellLocation.y);
        }
    }

    public void scrollToSelection() {
        final int rowCount = getRowCount();

        if (rowCount > 0) {
            int i = getSelectedRow();
            if (i < 0) {
                i = 0;
                setRowSelectionInterval(i, i);
            }
            if (i >= rowCount) {
                i = rowCount - 1;
            }
            scrollToSelection(i);
        }
    }

    private void scrollToSelection(int rowIndex) {
        if (!(getParent() instanceof JViewport)) {
            return;
        }
        JViewport viewport = (JViewport) getParent();
        Rectangle rect = getCellRect(rowIndex, 0, true);
        Rectangle viewRect = viewport.getViewRect();
        rect.setLocation(rect.x - viewRect.x, rect.y - viewRect.y);

        int centerX = (viewRect.width - rect.width) / 2;
        int centerY = (viewRect.height - rect.height) / 2;
        if (rect.x < centerX) {
            centerX = -centerX;
        }
        if (rect.y < centerY) {
            centerY = -centerY;
        }
        rect.translate(centerX, centerY);
        viewport.scrollRectToVisible(rect);
    }

    public void getSelected() {
        // Einstellungen der Tabelle merken
        selRow = getSelectedRow();
        selRows = getSelectedRows();
    }

    protected void setSelected() {
        if (selRows != null) {
            if (selRows.length > 0) {
                selectionModel.setValueIsAdjusting(true);
                for (int selectedRow : selRows) {
                    if (selectedRow < getRowCount()) {
                        addRowSelectionInterval(selectedRow, selectedRow);
                    }
                }
                selectionModel.setValueIsAdjusting(false);
            }
        }
    }

    private void changeColumnWidth2() {
        final TableColumnModel model = getColumnModel();
        for (int i = 0; i < breite.length && i < getColumnCount(); ++i) {
            final int colIndex = convertColumnIndexToView(i);
            if (breite[i] == 0) {
                model.getColumn(colIndex).setMinWidth(0);
                model.getColumn(colIndex).setPreferredWidth(0);
                model.getColumn(colIndex).setMaxWidth(0);
            } else {
                model.getColumn(colIndex).setMinWidth(10);
                model.getColumn(colIndex).setMaxWidth(3000);
                model.getColumn(colIndex).setPreferredWidth(breite[i]);
            }
        }
    }

    private void changeColumnWidth() {
        for (int i = 0; i < breite.length && i < getColumnCount(); ++i) {
            if (!anzeigen(i, spaltenAnzeigen)) {
                // geänderte Ansicht der Spalten abfragen
                breite[i] = 0;
            } else if (breite[i] == 0) {
                breite[i] = 100; // damit sie auch zu sehen ist :)
            }
        }
    }

    public void spaltenEinAus() {
        getSpalten(); // die aktuelle Breite holen
        changeColumnWidth();
        changeColumnWidth2();

        validate();
    }

    public void getSpalten() {
        // Einstellungen der Tabelle merken
        getSelected();

        for (int i = 0; i < reihe.length && i < getModel().getColumnCount(); ++i) {
            reihe[i] = convertColumnIndexToModel(i);
        }

        for (int i = 0; i < breite.length && i < getModel().getColumnCount(); ++i) {
            breite[i] = getColumnModel().getColumn(convertColumnIndexToView(i)).getWidth();
        }
        if (this.getRowSorter() != null) {
            listeSortKeys = getRowSorter().getSortKeys();
        } else {
            listeSortKeys = null;
        }
    }

    public void setSpalten() {
        // gemerkte Einstellungen der Tabelle wieder setzten
        try {
            changeColumnWidth();

            final TableColumnModel model = getColumnModel();
            changeColumnWidth2();

            for (int i = 0; i < reihe.length && i < getColumnCount(); ++i) {
                model.moveColumn(convertColumnIndexToView(reihe[i]), i);
            }

            if (listeSortKeys != null) {
                if (!listeSortKeys.isEmpty()) {
                    getRowSorter().setSortKeys(listeSortKeys);
                }
            }

            setSelected();

            validate();
        } catch (Exception ex) {
            Log.errorLog(965001463, ex);
        }
    }

    /**
     * Perform common reset steps for all subclasses.
     */
    public void resetTabelle() {
        listeSortKeys = null;

        getRowSorter().setSortKeys(null);
        setRowSorter(null);
        setAutoCreateRowSorter(true);
        spaltenAusschalten();
        setSpaltenEinAus(breite, spaltenAnzeigen);
        setSpalten();
        setHeight();
    }

    protected abstract void spaltenAusschalten();

    public void tabelleNachDatenSchreiben() {
        // Tabellendaten ind die Daten.system schreiben
        // erst die Breite, dann die Reihenfolge
        String b, r, s = "", upDown = "";
        int reihe_[] = new int[maxSpalten];
        int breite_[] = new int[maxSpalten];
        for (int i = 0; i < reihe_.length && i < getModel().getColumnCount(); ++i) {
            reihe_[i] = convertColumnIndexToModel(i);
        }

        final TableColumnModel model = getColumnModel();
        for (int i = 0; i < breite_.length && i < getModel().getColumnCount(); ++i) {
            breite_[i] = model.getColumn(convertColumnIndexToView(i)).getWidth();
        }

        b = Integer.toString(breite_[0]);
        r = Integer.toString(reihe_[0]);
        for (int i = 1; i < breite.length; i++) {
            b = b + ',' + Integer.toString(breite_[i]);
            r = r + ',' + Integer.toString(reihe_[i]);
        }

        listeSortKeys = this.getRowSorter().getSortKeys();
        if (listeSortKeys != null) {
            if (!listeSortKeys.isEmpty()) {
                SortKey sk = listeSortKeys.get(0);
                s = String.valueOf(sk.getColumn());
                upDown = sk.getSortOrder().equals(SortOrder.ASCENDING) ? SORT_ASCENDING : SORT_DESCENDING;
            }
        }

        MVConfig.add(nrDatenSystem, b + FELDTRENNER + r + FELDTRENNER + s + FELDTRENNER + upDown);
        if (iconAnzeigenStr != null) {
            MVConfig.add(iconAnzeigenStr, String.valueOf(iconAnzeigen));
        }
        if (iconKleinStr != null) {
            MVConfig.add(iconKleinStr, String.valueOf(iconKlein));
        }
    }

    private int[] getArray(int anzahl) {
        final int[] arr = new int[anzahl];
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
                if (!s.isEmpty()) {
                    if (s.contains(",")) {
                        sub = s.substring(0, s.indexOf(','));
                        s = s.replaceFirst(sub + ',', "");
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
        return ++ret;
    }

}
