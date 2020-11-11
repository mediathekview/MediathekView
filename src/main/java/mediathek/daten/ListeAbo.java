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
package mediathek.daten;

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.AboListChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FilenameUtils;
import mediathek.tool.Filter;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.MVMessageDialog;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.NoSuchElementException;

@SuppressWarnings("serial")
public class ListeAbo extends LinkedList<DatenAbo> {
    private static final String[] LEER = {""};
    private static final Logger logger = LogManager.getLogger(ListeAbo.class);
    private final Daten daten;
    private int nr;

    public ListeAbo(Daten ddaten) {
        daten = ddaten;
    }

    public void addAbo(String aboName) {
        addAbo(aboName, "", "", "");
    }

    public void addAbo(String aboname, String filmSender, String filmThema, String filmTitel) {
        int min;
        try {
            min = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE));
        } catch (Exception ex) {
            min = 0;
            MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, "0");
        }

        addAbo(filmSender, filmThema, filmTitel, "", "", min, true/*min*/, aboname);
    }

    public void addAbo(String filmSender, String filmThema, String filmTitel, String filmThemaTitel, String irgendwo, int mindestdauer, boolean min, String namePfad) {
        //abo anlegen, oder false wenns schon existiert
        namePfad = FilenameUtils.replaceLeerDateiname(namePfad, false /*nur ein Ordner*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
        DatenAbo datenAbo = new DatenAbo(namePfad /* name */, filmSender, filmThema, filmTitel, filmThemaTitel, irgendwo, mindestdauer, min, namePfad, "");
        DialogEditAbo dialogEditAbo = new DialogEditAbo(MediathekGui.ui(), true, datenAbo, false /*onlyOne*/);
        dialogEditAbo.setTitle("Neues Abo anlegen");
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.ok) {
            if (!aboExistiertBereits(datenAbo)) {
                MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, datenAbo.arr[DatenAbo.ABO_MINDESTDAUER]); // als Vorgabe merken
                addAbo(datenAbo);
                aenderungMelden();
                sort();
            } else {
                MVMessageDialog.showMessageDialog(null, "Abo existiert bereits", "Abo anlegen", JOptionPane.INFORMATION_MESSAGE);
            }
        }
    }

    public void addAbo(DatenAbo datenAbo) {
        // die Änderung an der Liste wird nicht gemeldet!!
        // für das Lesen der Konfig-Datei beim Programmstart
        ++nr;
        datenAbo.setNr(nr);
        if (datenAbo.arr[DatenAbo.ABO_NAME].isEmpty()) {
            // Downloads ohne "Aboname" sind manuelle Downloads
            datenAbo.arr[DatenAbo.ABO_NAME] = "Abo_" + nr;
        }
        datenAbo.setMindestDauerMinuten();
        if (datenAbo.arr[DatenAbo.ABO_MIN].isEmpty()) {
            //zum Erhalt der alten Funktionalität
            datenAbo.arr[DatenAbo.ABO_MIN] = Boolean.TRUE.toString();
        }
        datenAbo.min = Boolean.parseBoolean(datenAbo.arr[DatenAbo.ABO_MIN]);

        add(datenAbo);
    }

    public void aboLoeschen(@NotNull DatenAbo abo) {
            remove(abo);
            aenderungMelden();
    }

    /**
     * Get the number of abos which are active and used.
     * @return num of used abos
     */
    public long activeAbos() {
        return stream().filter(DatenAbo::aboIstEingeschaltet).count();
    }

    /**
     * Get the number of abos which are created but offline.
     * @return number of abos which are offline
     */
    public long inactiveAbos() {
        return stream().filter(abo -> !abo.aboIstEingeschaltet()).count();
    }

    public void aenderungMelden() {
        // Filmliste anpassen
        setAboFuerFilm(daten.getListeFilme(), true);
        daten.getMessageBus().publishAsync(new AboListChangedEvent());
    }

    @Deprecated
    /*
     * Dangerous code, returns abo by list index not by number.
     * Needs to be checked and recoded!!
     */
    public DatenAbo getAboNr(int i) {
        return this.get(i);
    }

    /**
     * Find an abo by its assigned number within the list.
     * @param nr the formerly assigned abo number.
     * @return the found abo.
     * @throws NoSuchElementException when no abo was found.
     */
    public DatenAbo findByNr(int nr) throws NoSuchElementException {
        return this.stream()
                .filter(abo -> abo.getNr() == nr)
                .findAny().orElseThrow();
    }

    public void sort() {
        Collections.sort(this);
    }

    public ArrayList<String> getPfade() {
        // liefert eine Array mit allen Pfaden
        ArrayList<String> pfade = new ArrayList<>();
        for (DatenAbo abo : this) {
            String s = abo.arr[DatenAbo.ABO_ZIELPFAD];
            if (!pfade.contains(s)) {
                pfade.add(abo.arr[DatenAbo.ABO_ZIELPFAD]);
            }
        }
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        pfade.sort(sorter);
        return pfade;
    }

    private boolean aboExistiertBereits(DatenAbo abo) {
        // true wenn es das Abo schon gibt
        for (DatenAbo datenAbo : this) {
            if (Filter.aboExistiertBereits(datenAbo, abo)) {
                return true;
            }
        }
        return false;
    }

    public DatenAbo getAboFuerFilm_schnell(DatenFilm film, boolean laengePruefen) {
        // da wird nur in der Filmliste geschaut, ob in "DatenFilm" ein Abo eingetragen ist
        // geht schneller, "getAboFuerFilm" muss aber vorher schon gelaufen sein!!
        final var abo = film.getAbo();
        if (abo == null) {
            return null;
        } else {
            if (laengePruefen) {
                if (!Filter.laengePruefen(abo.mindestdauerMinuten, film.getFilmLength(), abo.min)) {
                    return null;
                }
            }
            return abo;
        }
    }

    private void deleteAboInFilm(DatenFilm film) {
        // für jeden Film Abo löschen
        film.setAboName("");
        film.setAbo(null);
    }

    private void createAbo(DatenAbo abo) {
        if (abo.arr[DatenAbo.ABO_TITEL].isEmpty()) {
            abo.titel = LEER;
        } else {
            abo.titel = Filter.isPattern(abo.arr[DatenAbo.ABO_TITEL])
                    ? new String[]{abo.arr[DatenAbo.ABO_TITEL]} : abo.arr[DatenAbo.ABO_TITEL].toLowerCase().split(",");
        }
        if (abo.arr[DatenAbo.ABO_THEMA_TITEL].isEmpty()) {
            abo.thema = LEER;
        } else {
            abo.thema = Filter.isPattern(abo.arr[DatenAbo.ABO_THEMA_TITEL])
                    ? new String[]{abo.arr[DatenAbo.ABO_THEMA_TITEL]} : abo.arr[DatenAbo.ABO_THEMA_TITEL].toLowerCase().split(",");
        }
        if (abo.arr[DatenAbo.ABO_IRGENDWO].isEmpty()) {
            abo.irgendwo = LEER;
        } else {
            abo.irgendwo = Filter.isPattern(abo.arr[DatenAbo.ABO_IRGENDWO])
                    ? new String[]{abo.arr[DatenAbo.ABO_IRGENDWO]} : abo.arr[DatenAbo.ABO_IRGENDWO].toLowerCase().split(",");
        }
    }

    /**
     * Assign found abo to the film objects.
     * Time-intensive procedure!
     *
     * @param film assignee
     */
    private void assignAboToFilm(@NotNull DatenFilm film) {
        stream().filter(abo
                -> Filter.filterAufFilmPruefen(abo.arr[DatenAbo.ABO_SENDER], abo.arr[DatenAbo.ABO_THEMA],
                abo.titel,
                abo.thema,
                abo.irgendwo,
                film))
                .findFirst().
                ifPresentOrElse(foundAbo -> assignAboToFilm(foundAbo, film), () -> deleteAboInFilm(film));
    }

    private void assignAboToFilm(DatenAbo foundAbo, DatenFilm film) {
        if (!Filter.laengePruefen(foundAbo.mindestdauerMinuten, film.getFilmLength(), foundAbo.min)) {
            // dann ist der Film zu kurz
            film.setAboName(foundAbo.arr[DatenAbo.ABO_NAME] + (foundAbo.min ? " [zu kurz]" : " [zu lang]"));
        } else {
            film.setAboName(foundAbo.arr[DatenAbo.ABO_NAME]);
        }
        film.setAbo(foundAbo);
    }

    public void setAboFuerFilm(ListeFilme listeFilme, boolean aboLoeschen) {
        Stopwatch stopwatch = Stopwatch.createStarted();
        // hier wird tatsächlich für jeden Film die Liste der Abos durchsucht
        // braucht länger

        if (this.isEmpty() && aboLoeschen) {
            listeFilme.parallelStream().forEach(this::deleteAboInFilm);
            return;
        }

        // leere Abos löschen, die sind Fehler
        this.stream().filter(DatenAbo::isEmpty).forEach(this::remove);

        // und jetzt erstellen
        forEach(this::createAbo);

        // das kostet die Zeit!!
        listeFilme.parallelStream().forEach(this::assignAboToFilm);

        // und jetzt wieder löschen
        forEach(datenAbo -> {
            datenAbo.titel = LEER;
            datenAbo.thema = LEER;
            datenAbo.irgendwo = LEER;
        });

        stopwatch.stop();
        logger.debug("setAboFuerFilm: {}", stopwatch);
    }
}
