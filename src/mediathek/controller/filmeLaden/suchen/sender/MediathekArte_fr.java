/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.filmeLaden.suchen.sender;

import java.text.SimpleDateFormat;
import java.util.Locale;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;

public class MediathekArte_fr extends MediathekArte_de implements Runnable {

    /**
     *
     * @param ddaten
     * @param dde
     */
    public MediathekArte_fr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, startPrio);
        nameSenderFilmliste = SENDER_ARTE_FR;
        nameSenderMReader = SENDER_ARTE_FR;
        startUrl = "http://www.arte.tv/guide/fr/plus7.json?page=1&per_page=400&regions=default%2CEUR_DE_FR%2CDE_FR%2CSAT%2CALL";
        // "vendredi 02 août à  8h30"
        sdfIn = new SimpleDateFormat("EEEEddMMMHmm yyyy", Locale.FRANCE);
    }

    @Override
    String datumAendern(String datum) {
        // private static SimpleDateFormat sdfIn = new SimpleDateFormat("EEE, dd, MMM HH:mm", Locale.GERMANY);
        // "vendredi 02 août à  8h30"
        // "jeudi 01 août à 20h48"
        // Freitag, 02. August um  5:00 Uhr
        datum = datum.replace(" à", "");
        datum = datum.substring(0, datum.length() - 3) + datum.substring(datum.length() - 2);
        datum = datum.replace(" ", "");
        return datum + jahr;
    }
}
