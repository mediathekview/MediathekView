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

import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlisteLesen;
import mSearch.tool.Duration;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.tool.GuiFunktionen;

import java.text.SimpleDateFormat;
import java.util.Date;

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

            Daten daten = Daten.getInstance();

            new FilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            SysMsg.sysMsg("Liste Filme gelesen am: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            SysMsg.sysMsg("  erstellt am: " + daten.getListeFilme().genDate());
            SysMsg.sysMsg("  Anzahl Filme: " + daten.getListeFilme().size());
            SysMsg.sysMsg("  Anzahl Neue: " + daten.getListeFilme().countNewFilms());

            if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO && daten.getListeFilme().isTooOld()) {
                SysMsg.sysMsg("Filmliste zu alt, neue Filmliste laden");
                daten.getFilmeLaden().loadFilmlist("", true);
            } else {
                // beim Neuladen wird es dann erst gemacht
                daten.getFilmeLaden().notifyStart(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false/*Fehler*/));

                daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Themen suchen", 0, 0, 0, false/*Fehler*/));
                daten.getListeFilme().themenLaden();

                daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Abos eintragen", 0, 0, 0, false/*Fehler*/));
                daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false /*aboLoeschen*/);

                daten.getFilmeLaden().notifyProgress(new ListenerFilmeLadenEvent("", "Blacklist filtern", 0, 0, 0, false/*Fehler*/));
                daten.getListeBlacklist().filterListe();

                daten.getFilmeLaden().notifyFertig(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false/*Fehler*/));
            }
        }

    }
}
