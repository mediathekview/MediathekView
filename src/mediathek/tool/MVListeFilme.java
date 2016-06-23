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

import mediathek.daten.Daten;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;

public class MVListeFilme {

    public static void checkBlacklist() {
        Daten.listeBlacklist.filterListe(Daten.listeFilme, Daten.listeFilmeNachBlackList);
    }

    public static synchronized void getModelTabFilme(ListeFilme listeFilme, Daten ddaten, MVTable table,
            String filterSender, String filterThema, String filterTitel, String filterThemaTitel, String filterIrgendwo,
            int laenge, boolean keineAbos, boolean kGesehen, boolean nurHd, boolean live, boolean nurNeue) {
        // Model für die Tabelle Filme zusammenbauen
        if (listeFilme.isEmpty()) {
            // wenn die Liste leer ist, dann Tschüss
            table.setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
            return;
        }
        // dann ein neues Model anlegen
        TModel tModel = new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES);
        if (filterSender.isEmpty() && filterThema.isEmpty() && filterTitel.isEmpty() && filterThemaTitel.isEmpty() && filterIrgendwo.isEmpty() && laenge == 0
                && !keineAbos && !kGesehen && !nurHd && !live && !nurNeue) {
            // dann ganze Liste laden
            addObjectDataTabFilme(listeFilme, tModel);
        } else {
            // Titel
            String[] arrTitel;
            if (Filter.isPattern(filterTitel)) {
                arrTitel = new String[]{filterTitel};
            } else {
                arrTitel = filterTitel.split(",");
                for (int i = 0; i < arrTitel.length; ++i) {
                    arrTitel[i] = arrTitel[i].trim().toLowerCase();
                }
            }
            // ThemaTitel
            String[] arrThemaTitel;
            if (Filter.isPattern(filterThemaTitel)) {
                arrThemaTitel = new String[]{filterThemaTitel};
            } else {
                arrThemaTitel = filterThemaTitel.split(",");
                for (int i = 0; i < arrThemaTitel.length; ++i) {
                    arrThemaTitel[i] = arrThemaTitel[i].trim().toLowerCase();
                }
            }
            // Irgendwo
            String[] arrIrgendwo;
            if (Filter.isPattern(filterIrgendwo)) {
                arrIrgendwo = new String[]{filterIrgendwo};
            } else {
                arrIrgendwo = filterIrgendwo.split(",");
                for (int i = 0; i < arrIrgendwo.length; ++i) {
                    arrIrgendwo[i] = arrIrgendwo[i].trim().toLowerCase();
                }
            }
            for (DatenFilm film : listeFilme) {
                if (nurNeue) {
                    if (!film.isNew()) {
                        continue;
                    }
                }
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
                    if (ddaten.history.urlPruefen(film.getUrlHistory())) {
                        continue;
                    }
                }
                if (Filter.filterAufFilmPruefen(filterSender, filterThema, arrTitel, arrThemaTitel, arrIrgendwo, laenge, film, true /*länge nicht prüfen*/)) {
                    addObjectDataTabFilme(tModel, film);
                }
            }
            // listeFilme.stream().filter((DatenFilm film) -> Filter.filterAufFilmPruefen(filterSender,
            //      filterThema, arrTitel, arrThemaTitel, arrIrgendwo, laenge, film, true /*länge nicht prüfen*/)).forEach(f -> addObjectDataTabFilme(tModel, f));

        }
        table.setModel(tModel);
    }

//    public static synchronized void getModelTabFilme_(ListeFilme listeFilme, Daten ddaten, MVTable table,
//            String filterSender, String filterThema, String filterTitel, String filterThemaTitel, String filterIrgendwo,
//            int laenge, boolean keineAbos, boolean kGesehen, boolean nurHd, boolean live) {
//        // Model für die Tabelle Filme zusammenbauen
//        TModel tModel = (TModel) table.getModel();
//        tModel.setRowCount(0);
//        if (listeFilme.isEmpty()) {
//            // wenn die Liste leer ist, dann Tschüss
//            table.setModel(new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES));
//            return;
//        }
//        // dann ein neues Model anlegen
//        tModel.setRowCount(listeFilme.size());
//        if (filterSender.equals("") && filterThema.equals("") && filterTitel.equals("") && filterThemaTitel.equals("") && filterIrgendwo.equals("") && laenge == 0
//                && !keineAbos && !kGesehen && !nurHd && !live) {
//            // =============================================================
//            // wenn ganze Liste
//            int row = 0;
//            for (DatenFilm film : listeFilme) {
//                addObjectDataTabFilme(tModel, film, row);
//                ++row;
//            }
//            //tModel.setRowCount(row);
//        } else {
//            // =============================================================
//            // nur mit Filter laden
//            // Titel
//            String[] arrTitel;
//            if (Filter.isPattern(filterTitel)) {
//                arrTitel = new String[]{filterTitel};
//            } else {
//                arrTitel = filterTitel.split(",");
//                for (int i = 0; i < arrTitel.length; ++i) {
//                    arrTitel[i] = arrTitel[i].trim().toLowerCase();
//                }
//            }
//            // ThemaTitel
//            String[] arrThemaTitel;
//            if (Filter.isPattern(filterThemaTitel)) {
//                arrThemaTitel = new String[]{filterThemaTitel};
//            } else {
//                arrThemaTitel = filterThemaTitel.split(",");
//                for (int i = 0; i < arrThemaTitel.length; ++i) {
//                    arrThemaTitel[i] = arrThemaTitel[i].trim().toLowerCase();
//                }
//            }
//            // Irgendwo
//            String[] arrIrgendwo;
//            if (Filter.isPattern(filterIrgendwo)) {
//                arrIrgendwo = new String[]{filterIrgendwo};
//            } else {
//                arrIrgendwo = filterIrgendwo.split(",");
//                for (int i = 0; i < arrIrgendwo.length; ++i) {
//                    arrIrgendwo[i] = arrIrgendwo[i].trim().toLowerCase();
//                }
//            }
//            int row = 0;
//            for (DatenFilm film : listeFilme) {
//                if (live) {
//                    if (!film.arr[DatenFilm.FILM_THEMA_NR].equals(ListeFilme.THEMA_LIVE)) {
//                        continue;
//                    }
//                }
//                if (nurHd) {
//                    if (film.arr[DatenFilm.FILM_URL_HD_NR].isEmpty()) {
//                        continue;
//                    }
//                }
//                if (keineAbos) {
//                    if (!film.arr[DatenFilm.FILM_ABO_NAME_NR].isEmpty()) {
//                        continue;
//                    }
//                }
//                if (kGesehen) {
//                    if (ddaten.history.urlPruefen(film.getUrlHistory())) {
//                        continue;
//                    }
//                }
//                if (Filter.filterAufFilmPruefen(filterSender, filterThema, arrTitel, arrThemaTitel, arrIrgendwo, laenge, film, true /*länge nicht prüfen*/)) {
//                    addObjectDataTabFilme(tModel, film, row);
//                    ++row;
//                }
//            }
//            tModel.setRowCount(row);
//        }
//        tModel.fireTableDataChanged();
//    }
//    private static void addObjectDataTabFilme(TModel tModel, DatenFilm film, int row) {
//        for (int column = 0; column < DatenFilm.MAX_ELEM; ++column) {
//            if (column == DatenFilm.FILM_DATUM_NR) {
//                tModel.setValueAt(film.datumFilm, row, column);
//            } else if (column == DatenFilm.FILM_GROESSE_NR) {
//                tModel.setValueAt(film.dateigroesseL, row, column);
////            } else if (column == DatenFilm.FILM_URL_NR || column == DatenFilm.FILM_NR_NR) {
////                // Url und Nr immer füllen, egal ob angezeigt
////                tModel.setValueAt(film.arr[column], row, column);
////            } else if (!DatenFilm.anzeigen(column)) {
////                tModel.setValueAt("", row, column);
//            } else {
//                tModel.setValueAt(film.arr[column], row, column);
//            }
//        }
//    }
    //===================================
    // private
    //===================================
    private static void addObjectDataTabFilme(ListeFilme listefilme, TModel tModel) {
        if (!listefilme.isEmpty()) {
            for (DatenFilm film : listefilme) {
                addObjectDataTabFilme(tModel, film);
            }
        }
    }

    private static void addObjectDataTabFilme(TModel tModel, DatenFilm film) {
        Object[] object = new Object[DatenFilm.MAX_ELEM];
        for (int m = 0; m < DatenFilm.MAX_ELEM; ++m) {
            if (m == DatenFilm.FILM_NR_NR) {
                object[m] = film.nr;
            } else if (m == DatenFilm.FILM_DATUM_NR) {
                object[m] = film.datumFilm;
            } else if (m == DatenFilm.FILM_GROESSE_NR) {
                object[m] = film.dateigroesseL;
            } else if (m == DatenFilm.FILM_REF_NR) {
                object[m] = film;
            } else if (m == DatenFilm.FILM_NEU_NR) {
                object[m] = "";
            } else {
                object[m] = film.arr[m];
            }
        }
        tModel.addRow(object);
    }
}
