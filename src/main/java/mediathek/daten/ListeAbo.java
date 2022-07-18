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

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.abo.DatenAbo;
import mediathek.daten.abo.FilmLengthState;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.messages.AboListChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.*;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collections;

public class ListeAbo extends ArrayList<DatenAbo> {
    private static final String[] LEER = {""};
    private int nr;

    private int parseMinSize() {
        int min;
        try {
            min = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE));
        } catch (Exception ex) {
            min = 0;
            MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, "0");
        }
        return min;
    }

    public void addAbo(String aboname, String filmSender, String filmThema, String filmTitel) {
        //abo anlegen, oder false wenns schon existiert
        aboname = FilenameUtils.replaceLeerDateiname(aboname, false /*nur ein Ordner*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));

        DatenAbo datenAbo = new DatenAbo();
        datenAbo.setName(aboname);
        datenAbo.setSender(filmSender);
        datenAbo.setThema(filmThema);
        datenAbo.setTitle(filmTitel);
        datenAbo.setThemaTitel("");
        datenAbo.setIrgendwo("");
        datenAbo.setMindestDauerMinuten(parseMinSize());
        datenAbo.setFilmLengthState(FilmLengthState.MINIMUM);
        datenAbo.setZielpfad(aboname);
        datenAbo.setPsetName("");

        DialogEditAbo dialogEditAbo = new DialogEditAbo(MediathekGui.ui(), datenAbo, false);
        dialogEditAbo.setTitle("Neues Abo anlegen");
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.successful()) {
            if (!aboExistiertBereits(datenAbo)) {
                MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, Integer.toString(datenAbo.getMindestDauerMinuten())); // als Vorgabe merken
                addAbo(datenAbo);
                aenderungMelden();
                Collections.sort(this);
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
        if (datenAbo.getName().isEmpty()) {
            // Downloads ohne "Aboname" sind manuelle Downloads
            datenAbo.setName("Abo_" + nr);
        }

        add(datenAbo);
    }

    public void aboLoeschen(@NotNull DatenAbo abo) {
        remove(abo);
        aenderungMelden();
    }

    public void aenderungMelden() {
        // Filmliste anpassen
        setAboFuerFilm(Daten.getInstance().getListeFilme(), true);
        MessageBus.getMessageBus().publishAsync(new AboListChangedEvent());
    }

    public ArrayList<String> getPfade() {
        // liefert eine Array mit allen Pfaden
        ArrayList<String> pfade = new ArrayList<>();
        for (DatenAbo abo : this) {
            final String zielpfad = abo.getZielpfad();
            if (!pfade.contains(zielpfad)) {
                pfade.add(zielpfad);
            }
        }

        pfade.sort(GermanStringSorter.getInstance());
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
                if (!Filter.laengePruefen(abo.getMindestDauerMinuten(), film.getFilmLength(),
                        abo.getFilmLengthState() == FilmLengthState.MINIMUM)) {
                    return null;
                }
            }
            return abo;
        }
    }

    private void deleteAboInFilm(DatenFilm film) {
        // für jeden Film Abo löschen
        film.setAbo(null);
    }

    private void createAbo(DatenAbo abo) {
        if (abo.getTitle().isEmpty()) {
            abo.setTitelFilterPattern(LEER);
        } else {
            abo.setTitelFilterPattern(Filter.isPattern(abo.getTitle())
                    ? new String[]{abo.getTitle()} : abo.getTitle().toLowerCase().split(","));
        }
        if (abo.getThemaTitel().isEmpty()) {
            abo.setThemaFilterPattern(LEER);
        } else {
            abo.setThemaFilterPattern(Filter.isPattern(abo.getThemaTitel())
                    ? new String[]{abo.getThemaTitel()} : abo.getThemaTitel().toLowerCase().split(","));
        }
        if (abo.getIrgendwo().isEmpty()) {
            abo.setIrgendwoFilterPattern(LEER);
        } else {
            abo.setIrgendwoFilterPattern(Filter.isPattern(abo.getIrgendwo())
                    ? new String[]{abo.getIrgendwo()} : abo.getIrgendwo().toLowerCase().split(","));
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
                -> Filter.filterAufFilmPruefen(abo.getSender(), abo.getThema(),
                abo.getTitelFilterPattern(),
                abo.getThemaFilterPattern(),
                abo.getIrgendwoFilterPattern(),
                film))
                .findAny().
                ifPresentOrElse(film::setAbo, () -> deleteAboInFilm(film));
    }

    /**
     * Hier wird tatsächlich für jeden Film die Liste der Abos durchsucht.
     * Braucht länger.
     * @param listeFilme Die Filmliste
     * @param aboLoeschen abo löschen?
     */
    public void setAboFuerFilm(ListeFilme listeFilme, boolean aboLoeschen) {
        if (this.isEmpty() && aboLoeschen) {
            listeFilme.forEach(this::deleteAboInFilm);
            return;
        }

        // leere Abos löschen, die sind Fehler
        this.stream().filter(DatenAbo::isInvalid).forEach(this::remove);

        // und jetzt erstellen
        forEach(this::createAbo);

        // das kostet die Zeit!!
        listeFilme.parallelStream().forEach(this::assignAboToFilm);

        // und jetzt wieder löschen
        forEach(datenAbo -> {
            datenAbo.setTitelFilterPattern(LEER);
            datenAbo.setThemaFilterPattern(LEER);
            datenAbo.setIrgendwoFilterPattern(LEER);
        });
    }
}
