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

import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mediathek.daten.Daten;

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
                    if (!film.arr[DatenFilm.FILM_THEMA].equals(ListeFilme.THEMA_LIVE)) {
                        continue;
                    }
                }
                if (nurHd) {
                    if (!film.isHD()) {
                        continue;
                    }
                }
                if (keineAbos) {
                    if (!film.arr[DatenFilm.FILM_ABO_NAME].isEmpty()) {
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
            switch (m) {
                case DatenFilm.FILM_NR:
                    object[m] = film.nr;
                    break;
                case DatenFilm.FILM_DATUM:
                    object[m] = film.datumFilm;
                    break;
                case DatenFilm.FILM_GROESSE:
                    object[m] = film.dateigroesseL;
                    break;
                case DatenFilm.FILM_REF:
                    object[m] = film;
                    break;
                case DatenFilm.FILM_NEU:
                    object[m] = film.isNew() ? "1" : "0";
                    break;
                case DatenFilm.FILM_HD:
                    object[m] = film.isHD() ? "1" : "0";
                    break;
                case DatenFilm.FILM_UT:
                    object[m] = film.hasUT() ? "1" : "0";
                    break;
                default:
                    object[m] = film.arr[m];
                    break;
            }
        }
        tModel.addRow(object);
    }
}
