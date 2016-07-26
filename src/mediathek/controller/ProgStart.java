/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.controller;

import java.text.SimpleDateFormat;
import java.util.Date;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlisteLesen;
import mSearch.tool.Duration;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.tool.GuiFunktionen;

public class ProgStart {
    // #########################################################
    // Filmliste beim Programmstart!! laden
    // #########################################################

    public static void loadDataProgStart() {
        // Gui startet ein wenig flüssiger
        new Thread(new loadFilmlistProgStart_()).start();
    }

    private static class loadFilmlistProgStart_ implements Runnable {

        @Override
        public synchronized void run() {
            Duration.staticPing("Programmstart Daten laden");

            new FilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), Daten.listeFilme, Integer.parseInt(MVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
            SysMsg.sysMsg("Liste Filme gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            SysMsg.sysMsg("  erstellt am: " + Daten.listeFilme.genDate());
            SysMsg.sysMsg("  Anzahl Filme: " + Daten.listeFilme.size());
            SysMsg.sysMsg("  Anzahl Neue: " + Daten.listeFilme.countNewFilms());

            MVConfig.add(MVConfig.SYSTEM_BLACKLIST_ON, MVConfig.get(MVConfig.SYSTEM_BLACKLIST_START_ON)); // Zustand Blacklist beim Start setzen

            if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO && Daten.listeFilme.isTooOld()) {
                SysMsg.sysMsg("Filmliste zu alt, neue Filmliste laden");
                Daten.filmeLaden.loadFilmlist("", true);
            } else {
                // beim Neuladen wird es dann erst gemacht
                Daten.filmeLaden.notifyStart(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false/*Fehler*/));

                Daten.filmeLaden.notifyProgress(new ListenerFilmeLadenEvent("", "Themen suchen", 0, 0, 0, false/*Fehler*/));
                Daten.listeFilme.themenLaden();

                Daten.filmeLaden.notifyProgress(new ListenerFilmeLadenEvent("", "Abos eintragen", 0, 0, 0, false/*Fehler*/));
                Daten.listeAbo.setAboFuerFilm(Daten.listeFilme, false /*aboLoeschen*/);

                Daten.filmeLaden.notifyProgress(new ListenerFilmeLadenEvent("", "Blacklist filtern", 0, 0, 0, false/*Fehler*/));
                Daten.listeBlacklist.filterListe();

                Daten.filmeLaden.notifyFertig(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false/*Fehler*/));
            }
        }

    }
}
