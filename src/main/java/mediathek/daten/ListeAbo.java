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

import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.*;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.tool.Filter;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TModelAbo;

import javax.swing.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;

@SuppressWarnings("serial")
public class ListeAbo extends LinkedList<DatenAbo> {
    private final Daten daten;
    private static final String[] LEER = {""};
    //private String[] titel, thema, irgendwo;

    public ListeAbo(Daten ddaten) {
        daten = ddaten;
    }

    private int nr;

    public boolean addAbo(String aboName) {
        return addAbo(aboName, "", "", "");
    }

    public boolean addAbo(String aboname, String filmSender, String filmThema, String filmTitel) {
        int min;
        try {
            min = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE));
        } catch (Exception ex) {
            min = 0;
            MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, "0");
        }
        return addAbo(filmSender, filmThema, filmTitel, "", "", min, true/*min*/, aboname);
    }

    public boolean addAbo(String filmSender, String filmThema, String filmTitel, String filmThemaTitel, String irgendwo, int mindestdauer, boolean min, String namePfad) {
        //abo anlegen, oder false wenns schon existiert
        boolean ret = false;
        namePfad = FilenameUtils.replaceLeerDateiname(namePfad, false /*nur ein Ordner*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII)));
        DatenAbo datenAbo = new DatenAbo(namePfad /* name */, filmSender, filmThema, filmTitel, filmThemaTitel, irgendwo, mindestdauer, min, namePfad, "");
        DialogEditAbo dialogEditAbo = new DialogEditAbo(Daten.getInstance().getMediathekGui(), true, daten, datenAbo, false /*onlyOne*/);
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.ok) {
            if (!aboExistiertBereits(datenAbo)) {
                MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, datenAbo.arr[DatenAbo.ABO_MINDESTDAUER]); // als Vorgabe merken
                addAbo(datenAbo);
                aenderungMelden();
                sort();
                ret = true;
            } else {
                MVMessageDialog.showMessageDialog(null, "Abo existiert bereits", "Abo anlegen", JOptionPane.INFORMATION_MESSAGE);
            }
        }
        return ret;
    }

    public void addAbo(DatenAbo datenAbo) {
        // die Änderung an der Liste wird nicht gemeldet!!
        // für das Lesen der Konfig-Datei beim Programmstart
        ++nr;
        datenAbo.nr = nr;
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
        super.add(datenAbo);
    }

    public void aboLoeschen(DatenAbo abo) {
        if (abo != null) {
            this.remove(abo);
            aenderungMelden();
        }
    }

    public void aenderungMelden() {
        // Filmliste anpassen
        setAboFuerFilm(Daten.getInstance().getListeFilme(), true /*aboLoeschen*/);
        Listener.notify(Listener.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
    }

    public DatenAbo getAboNr(int i) {
        return this.get(i);
    }

    public void sort() {
        Collections.sort(this);
    }

    public void addObjectData(TModelAbo model, String sender) {
        model.setRowCount(0);
        Object[] object = new Object[DatenAbo.MAX_ELEM];
        for (DatenAbo datenAbo : this) {
            if (sender.isEmpty() || sender.equals(datenAbo.arr[DatenAbo.ABO_SENDER])) {
                for (int m = 0; m < DatenAbo.MAX_ELEM; ++m) {
                    if (m == DatenAbo.ABO_NR) {
                        object[m] = datenAbo.nr;
                    } else if (m == DatenAbo.ABO_MINDESTDAUER) {
                        object[m] = datenAbo.mindestdauerMinuten;
                    } else if (m == DatenAbo.ABO_DOWN_DATUM) {
                        object[m] = getDatumForObject(datenAbo.arr[DatenAbo.ABO_DOWN_DATUM]);
                    } else if (m == DatenAbo.ABO_EINGESCHALTET) {
                        object[m] = ""; //Boolean.valueOf(datenAbo.aboIstEingeschaltet());
                    } else if (m == DatenAbo.ABO_MIN) {
                        object[m] = datenAbo.min ? "min" : "max";
                    } else if (m != DatenAbo.ABO_NAME && !DatenAbo.anzeigen(m)) {
                        // Name immer füllen, egal ob angezeigt
                        object[m] = "";
                    } else {
                        object[m] = datenAbo.arr[m];
                    }
                }
                model.addRow(object);
            }
        }
    }

    public Datum getDatumForObject(String datum) {
        Datum tmp = new Datum(0);
        if (!datum.equals("")) {
            try {
                tmp.setTime(new SimpleDateFormat("dd.MM.yyyy").parse(datum).getTime());
            } catch (ParseException ignore) {
            }
        }
        return tmp;
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
        if (film.abo == null) {
            return null;
        } else {
            if (laengePruefen) {
                if (!Filter.laengePruefen(((DatenAbo) film.abo).mindestdauerMinuten, film.dauerL, ((DatenAbo) film.abo).min)) {
                    return null;
                }
            }
            return (DatenAbo) film.abo;
        }
    }

    //    public void setAboFuerFilm__(ListeFilme listeFilme, boolean aboLoeschen) {
//        // hier wird tatsächlich für jeden Film die Liste der Abos durchsucht
//        // braucht länger
//        Duration.counterStart("Abo in Filmliste eintragen ***ALT***");
//        DatenFilm datenFilm;
//        Iterator<DatenFilm> iteratorFilm = listeFilme.iterator();
//        DatenAbo datenAbo;
//        Iterator<DatenAbo> iteratorAbo;
//        if (this.isEmpty() && aboLoeschen) {
//            while (iteratorFilm.hasNext()) {
//                // für jeden Film Abo löschen
//                datenFilm = iteratorFilm.next();
//                datenFilm.arr[DatenFilm.FILM_ABO_NAME] = "";
//                datenFilm.abo = null;
//            }
//        } else {
//            while (iteratorFilm.hasNext()) {
//                // für jeden Film
//                datenFilm = iteratorFilm.next();
//                datenFilm.arr[DatenFilm.FILM_ABO_NAME] = "";
//                datenFilm.abo = null;
//                iteratorAbo = this.iterator();
//                while (iteratorAbo.hasNext()) {
//                    // jedes Abo prüfen
//                    datenAbo = iteratorAbo.next();
//                    if (datenAbo.isEmpty()) {
//                        //dann ists kein richtiges Abo!!
//                        continue;
//                    }
//                    if (datenAbo.arr[DatenAbo.ABO_TITEL].isEmpty()) {
//                        titel = LEER;
//                    } else {
//                        titel = Filter.isPattern(datenAbo.arr[DatenAbo.ABO_TITEL])
//                                ? new String[]{datenAbo.arr[DatenAbo.ABO_TITEL]} : datenAbo.arr[DatenAbo.ABO_TITEL].toLowerCase().split(",");
//                    }
//                    if (datenAbo.arr[DatenAbo.ABO_THEMA_TITEL].isEmpty()) {
//                        thema = LEER;
//                    } else {
//                        thema = Filter.isPattern(datenAbo.arr[DatenAbo.ABO_THEMA_TITEL])
//                                ? new String[]{datenAbo.arr[DatenAbo.ABO_THEMA_TITEL]} : datenAbo.arr[DatenAbo.ABO_THEMA_TITEL].toLowerCase().split(",");
//                    }
//                    if (datenAbo.arr[DatenAbo.ABO_IRGENDWO].isEmpty()) {
//                        irgendwo = LEER;
//                    } else {
//                        irgendwo = Filter.isPattern(datenAbo.arr[DatenAbo.ABO_IRGENDWO])
//                                ? new String[]{datenAbo.arr[DatenAbo.ABO_IRGENDWO]} : datenAbo.arr[DatenAbo.ABO_IRGENDWO].toLowerCase().split(",");
//                    }
//                    if (Filter.filterAufFilmPruefen(datenAbo.arr[DatenAbo.ABO_SENDER], datenAbo.arr[DatenAbo.ABO_THEMA],
//                            titel,
//                            thema,
//                            irgendwo,
//                            datenAbo.mindestdauerMinuten,
//                            datenFilm, false)) {
//                        // das Abo im Film eintragen
//                        // und noch die Filmlänge prüfen
//                        if (!Filter.laengePruefen(datenAbo.mindestdauerMinuten, datenFilm.dauerL)) {
//                            // dann ist der Film zu kurz
//                            datenFilm.arr[DatenFilm.FILM_ABO_NAME] = datenAbo.arr[DatenAbo.ABO_NAME] + " [zu kurz]";
//                            datenFilm.abo = datenAbo;
//                        } else {
//                            datenFilm.arr[DatenFilm.FILM_ABO_NAME] = datenAbo.arr[DatenAbo.ABO_NAME];
//                            datenFilm.abo = datenAbo;
//                        }
//                        // und nichts wie weiter
//                        break;
//                    }
//                }
//            }
//        }
//        Duration.counterStop("Abo in Filmliste eintragen ***ALT***");
//    }

    private void deleteAboInFilm(DatenFilm film) {
        // für jeden Film Abo löschen
        film.arr[DatenFilm.FILM_ABO_NAME] = "";
        film.abo = null;
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
    private void assignAboToFilm(DatenFilm film) {
        final DatenAbo foundAbo = this.stream().filter(abo
                -> Filter.filterAufFilmPruefen(abo.arr[DatenAbo.ABO_SENDER], abo.arr[DatenAbo.ABO_THEMA],
                abo.titel,
                abo.thema,
                abo.irgendwo,
                abo.mindestdauerMinuten,
                abo.min,
                film, false)).findFirst().orElse(null);

        if (foundAbo != null) {
            if (!Filter.laengePruefen(foundAbo.mindestdauerMinuten, film.dauerL, foundAbo.min)) {
                // dann ist der Film zu kurz
                film.arr[DatenFilm.FILM_ABO_NAME] = foundAbo.arr[DatenAbo.ABO_NAME] + (foundAbo.min ? " [zu kurz]" : " [zu lang]");
                film.abo = foundAbo;
            } else {
                film.arr[DatenFilm.FILM_ABO_NAME] = foundAbo.arr[DatenAbo.ABO_NAME];
                film.abo = foundAbo;
            }
        } else {
            deleteAboInFilm(film);
        }
    }

    public void setAboFuerFilm(ListeFilme listeFilme, boolean aboLoeschen) {
        // hier wird tatsächlich für jeden Film die Liste der Abos durchsucht
        // braucht länger

        Duration.counterStart("Abo in Filmliste eintragen");

        if (this.isEmpty() && aboLoeschen) {
            listeFilme.parallelStream().forEach(this::deleteAboInFilm);
            return;
        }

        // leere Abos löschen, die sind Fehler
        this.stream().filter((datenAbo) -> (datenAbo.isEmpty())).forEach(this::remove);

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

        Duration.counterStop("Abo in Filmliste eintragen");
    }
}
