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

import mSearch.daten.ListeFilme;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmListReader;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.filmlisten.FilmeLaden;
import mediathek.gui.messages.FilmListReadStartEvent;
import mediathek.gui.messages.FilmListReadStopEvent;
import mediathek.tool.GuiFunktionen;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.TimeUnit;

public class UIFilmlistLoaderThread extends Thread {

    private static final Logger logger = LogManager.getLogger(UIFilmlistLoaderThread.class);

    public UIFilmlistLoaderThread() {
        setName("UIFilmlistLoaderThread");
    }

    @Override
    public void run() {
        logger.debug("Programmstart Daten laden");
        try {
            //give the UI some time to set up...
            TimeUnit.MILLISECONDS.sleep(500);

            final Daten daten = Daten.getInstance();
            final ListeFilme listeFilme = daten.getListeFilme();
            final FilmeLaden filmeLaden = daten.getFilmeLaden();
            final int tageFilmliste = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE));

            daten.getMessageBus().publishAsync(new FilmListReadStartEvent());

            try (FilmListReader reader = new FilmListReader()) {
                reader.readFilmListe(Daten.getDateiFilmliste(), listeFilme, tageFilmliste);
                logger.info("Liste Filme gelesen am: {}", new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
                logger.info("  erstellt am: {}", listeFilme.genDate());
                logger.info("  Anzahl Filme: {}", listeFilme.size());
                logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms());

            }

            if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUTO && listeFilme.isTooOld()) {
                logger.info("Filmliste zu alt, neue Filmliste laden");
                filmeLaden.loadFilmlist("", true);
            } else {
                // beim Neuladen wird es dann erst gemacht
                filmeLaden.notifyStart(new ListenerFilmeLadenEvent("", "", 0, 0, 0, false/*Fehler*/));

                filmeLaden.notifyProgress(new ListenerFilmeLadenEvent("", "Themen suchen", 0, 0, 0, false/*Fehler*/));
                listeFilme.themenLaden();

                filmeLaden.notifyProgress(new ListenerFilmeLadenEvent("", "Abos eintragen", 0, 0, 0, false/*Fehler*/));
                daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false /*aboLoeschen*/);

                filmeLaden.notifyProgress(new ListenerFilmeLadenEvent("", "Blacklist filtern", 0, 0, 0, false/*Fehler*/));
                daten.getListeBlacklist().filterListe();

                filmeLaden.notifyFertig(new ListenerFilmeLadenEvent("", "", 100, 100, 0, false/*Fehler*/));
            }

            daten.getMessageBus().publishAsync(new FilmListReadStopEvent());
        } catch (InterruptedException ignored) {
        }

        logger.debug("Programmende Daten laden");

    }

}
