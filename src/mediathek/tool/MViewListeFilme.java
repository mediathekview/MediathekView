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

import java.util.HashSet;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.TreeSet;
import mediathek.daten.DDaten;
import mediathek.daten.DatenAbo;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;

public class MViewListeFilme {

    public static HashSet<String> hashSet = new HashSet<>();
    public static TreeSet<String> treeSet = new TreeSet<>(msearch.tool.GermanStringSorter.getInstance());

    public static synchronized void getModelTabFilme(ListeFilme listeFilme, DDaten ddaten, MVJTable table,
            String filterSender, String filterThema, String filterTitel, String filterThemaTitel, String filterIrgendwo,
            int laenge, boolean keineAbos, boolean kGesehen, boolean nurHd, boolean live) {
        // Model für die Tabelle Filme zusammenbauen
        TModel tModel = (TModel) table.getModel();
        if (listeFilme.size() == 0) {
            // wenn die Liste leer ist, dann Model löschen und Tschüss
            tModel.setRowCount(0);
            return;
        }
        if (tModel.getRowCount() != listeFilme.size()) {
            // dann ein neues Model anlegen
            tModel = new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES);
            tModel.setRowCount(listeFilme.size());
        }
        if (filterSender.equals("") && filterThema.equals("") && filterTitel.equals("") && filterThemaTitel.equals("") && filterIrgendwo.equals("") && laenge == 0
                && keineAbos == false && kGesehen == false && nurHd == false && live == false) {
            // wenn ganze Liste
            addObjectDataTabFilme(listeFilme, table);
        } else {
            // Titel
            String[] arrTitel;
            if (Filter.isPattern(filterTitel)) {
                arrTitel = new String[]{filterTitel};
            } else {
                arrTitel = filterTitel.split(",");
                for (int i = 0; i < arrTitel.length; ++i) {
                    arrTitel[i] = arrTitel[i].trim();
                }
            }
            // ThemaTitel
            String[] arrThemaTitel;
            if (Filter.isPattern(filterThemaTitel)) {
                arrThemaTitel = new String[]{filterThemaTitel};
            } else {
                arrThemaTitel = filterThemaTitel.split(",");
                for (int i = 0; i < arrThemaTitel.length; ++i) {
                    arrThemaTitel[i] = arrThemaTitel[i].trim();
                }
            }
            // Irgendwo
            String[] arrIrgendwo;
            if (Filter.isPattern(filterIrgendwo)) {
                arrIrgendwo = new String[]{filterIrgendwo};
            } else {
                arrIrgendwo = filterIrgendwo.split(",");
                for (int i = 0; i < arrIrgendwo.length; ++i) {
                    arrIrgendwo[i] = arrIrgendwo[i].trim();
                }
            }
            DatenFilm film;
            int row = 0;
            Iterator<DatenFilm> it = listeFilme.iterator();
            while (it.hasNext()) {
                film = it.next();
                if (live) {
                    if (!film.arr[DatenFilm.FILM_THEMA_NR].equals(ListeFilme.THEMA_LIVE)) {
                        continue;
                    }
                }
                if (nurHd) {
                    if (film.arr[DatenFilm.FILM_URL_HD_NR].isEmpty()) {
                        continue;
                    }
                }
                if (keineAbos) {
                    if (!film.arr[DatenFilm.FILM_ABO_NAME_NR].isEmpty()) {
                        continue;
                    }
                }
                if (kGesehen) {
                    if (ddaten.history.contains(film.arr[DatenFilm.FILM_URL_NR])) {
                        continue;
                    }
                }
                if (Filter.filterAufFilmPruefen(filterSender, filterThema, arrTitel, arrThemaTitel, arrIrgendwo, laenge, film, true /*länge nicht prüfen*/)) {
                    addObjectDataTabFilme(tModel, film, row++);
                }
            }
        }
    }

    public static synchronized String[] getModelOfFieldThema(ListeFilme listeFilme, String sender) {
        // erstellt ein StringArray der Themen eines Senders oder wenn "sender" leer, aller Sender
        // ist für die Filterfelder im GuiFilme
        // doppelte Einträge (bei der Groß- und Kleinschribung) werden entfernt
        String str, s;
        treeSet.add("");
        DatenFilm film;
        Iterator<DatenFilm> it = listeFilme.iterator();
        if (sender.equals("")) {
            //alle Theman
            while (it.hasNext()) {
                str = it.next().arr[DatenFilm.FILM_THEMA_NR];
                //hinzufügen
                s = str.toLowerCase();
                if (!hashSet.contains(s)) {
                    hashSet.add(s);
                    treeSet.add(str);
                }
            }
        } else {
            //nur Theman des Senders
            while (it.hasNext()) {
                film = it.next();
                if (film.arr[DatenFilm.FILM_SENDER_NR].equals(sender)) { // Filterstring ist immer "Sender"
                    //hinzufügen
                    str = film.arr[DatenFilm.FILM_THEMA_NR];
                    s = str.toLowerCase();
                    if (!hashSet.contains(s)) {
                        hashSet.add(s);
                        treeSet.add(str);
                    }
                }
            }
        }
        hashSet.clear();
        String[] a = treeSet.toArray(new String[]{});
        treeSet.clear();
        return a;
    }

    public static synchronized String[] getModelOfFieldSender(ListeFilme listeFilme) {
        // erstellt ein StringArray mit den Sendernamen
        String str;
        treeSet.add("");
        Iterator<DatenFilm> it = listeFilme.iterator();
        // Sendernamen gibts nur in einer Schreibweise
        int max = DDaten.filmeLaden.getSenderNamen().length; // gibt nur so viele
        while (it.hasNext()) {
            str = it.next().arr[DatenFilm.FILM_SENDER_NR];
            if (!treeSet.contains(str)) {
                treeSet.add(str);
                if (treeSet.size() > max) { // eins mehr wegen Leerzeile
                    break;
                }
            }
        }
        String[] a = treeSet.toArray(new String[]{});
        treeSet.clear();
        return a;
    }

    //===================================
    // private
    //===================================
    private static void addObjectDataTabFilme(ListeFilme listefilme, MVJTable table) {
        DatenFilm film;
        int row = 0;
        TModel tModel = (TModel) table.getModel();
        if (listefilme.size() > 0) {
            Iterator<DatenFilm> iterator = listefilme.iterator();
            while (iterator.hasNext()) {
                film = iterator.next();
                addObjectDataTabFilme(tModel, film, row);
                ++row;
            }
        }
    }

    private static void addObjectDataTabFilme(TModel tModel, DatenFilm film, int row) {
//        Object[] object = new Object[DatenFilm.MAX_ELEM];
//        for (int m = 0; m < DatenFilm.MAX_ELEM; ++m) {
//            if (m == DatenFilm.FILM_DATUM_NR) {
//                object[m] = film.datumFilm;
//            } else if (m == DatenFilm.FILM_GROESSE_NR) {
//                object[m] = film.dateigroesseL;
//            } else if (m != DatenFilm.FILM_URL_NR && m != DatenFilm.FILM_NR_NR && !DatenFilm.anzeigen(m)) {
//                // Url und Nr immer füllen, egal ob angezeigt
//                object[m] = "";
//            } else {
//                object[m] = film.arr[m];
//            }
//        }
//        model.addRow(object);
        for (int column = 0; column < DatenFilm.MAX_ELEM; ++column) {
            if (film.arr[column] == null) {
                System.out.println();
            }
            if (column == DatenFilm.FILM_DATUM_NR) {
                tModel.setValueAt(film.datumFilm, row, column);
            } else if (column == DatenFilm.FILM_GROESSE_NR) {
                tModel.setValueAt(film.dateigroesseL, row, column);
            } else if (column != DatenFilm.FILM_URL_NR && column != DatenFilm.FILM_NR_NR && !DatenFilm.anzeigen(column)) {
                // Url und Nr immer füllen, egal ob angezeigt
                tModel.setValueAt("", row, column);
            } else {
                tModel.setValueAt(film.arr[column], row, column);
            }
        }
    }

    public static void abosEintragen(ListeFilme listeFilme, DDaten ddaten) {
        // Aboname in die Filmliste eintragen
        DatenFilm film;
        DatenAbo datenAbo;
        ListIterator<DatenFilm> iterator = listeFilme.listIterator(0);
        while (iterator.hasNext()) {
            film = iterator.next();
            datenAbo = ddaten.listeAbo.getAboFuerFilm(film, false);
            if (datenAbo != null) {
                film.arr[DatenFilm.FILM_ABO_NAME_NR] = datenAbo.arr[DatenAbo.ABO_NAME_NR];
                // und jetzt noch die Filmlänge prüfen
                if (!Filter.laengePruefen(datenAbo.mindestdauerMinuten, film.dauerL)) {
                    // dann ist der Film zu kurz
                    film.arr[DatenFilm.FILM_ABO_NAME_NR] = film.arr[DatenFilm.FILM_ABO_NAME_NR] + " [zu kurz]";
                }
            } else {
                film.arr[DatenFilm.FILM_ABO_NAME_NR] = "";
            }
        }
    }
}
