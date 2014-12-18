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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.ListIterator;
import javax.swing.JOptionPane;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TModelAbo;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;
import msearch.tool.Datum;
import msearch.tool.GermanStringSorter;

public class ListeAbo extends LinkedList<DatenAbo> {

    Daten daten;
    final String[] LEER = {""};
    String[] titel, thema, irgendwo;

    public ListeAbo(Daten ddaten) {
        daten = ddaten;
    }
    private int nr = 0;

    public boolean addAbo(String filmSender, String filmThema, String filmTitel) {
        return addAbo(filmSender, filmThema, filmTitel, "", "", 0, filmThema);

    }

    public boolean addAbo(String filmSender, String filmThema, String filmTitel, String filmThemaTitel, String irgendwo, int mindestdauer, String namePfad) {
        //abo anlegen, oder false wenns schon existiert
        boolean ret = false;
        namePfad = GuiFunktionen.replaceLeerDateiname(namePfad);
        DatenAbo datenAbo = new DatenAbo(namePfad /* name */, filmSender, filmThema, filmTitel, filmThemaTitel, irgendwo, mindestdauer, namePfad, "");
        DialogEditAbo dialogEditAbo = new DialogEditAbo(null, true, daten, datenAbo);
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.ok) {
            if (!aboExistiertBereits(datenAbo)) {
                addAbo(datenAbo);
                aenderungMelden();
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
//        String str = String.valueOf(nr);
//        while (str.length() < 3) {
//            str = "0" + str;
//        }
//        datenAbo.arr[DatenAbo.ABO_NR_NR] = str;
        datenAbo.setMindestDauerMinuten();
        super.add(datenAbo);
        sort();
    }

    public void aboLoeschen(DatenAbo abo) {
        if (abo != null) {
            this.remove(abo);
            aenderungMelden();
        }
    }

    public void aenderungMelden() {
        // Filmliste anpassen
        setAboFuerFilm(Daten.listeFilme, true /*aboLoeschen*/);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
    }

    public DatenAbo getAboNr(int i) {
        return this.get(i);
    }

    public void sort() {
        Collections.sort(this);
    }

    public void addObjectData(TModelAbo model) {
        Object[] object;
        DatenAbo datenAbo;
        model.setRowCount(0);
        ListIterator<DatenAbo> iterator = this.listIterator();
        object = new Object[DatenAbo.MAX_ELEM];
        while (iterator.hasNext()) {
            datenAbo = iterator.next();
            //object[i] = datenAbo.arr;
            for (int m = 0; m < DatenAbo.MAX_ELEM; ++m) {
                if (m == DatenAbo.ABO_NR_NR) {
                    object[m] = datenAbo.nr;
                } else if (m == DatenAbo.ABO_DOWN_DATUM_NR) {
                    object[m] = getDatumForObject(datenAbo.arr[DatenAbo.ABO_DOWN_DATUM_NR]);
                } else if (m == DatenAbo.ABO_EINGESCHALTET_NR) {
                    object[m] = ""; //Boolean.valueOf(datenAbo.aboIstEingeschaltet());
                } else if (m != DatenAbo.ABO_NAME_NR && !DatenAbo.anzeigen(m)) {
                    // Name immer füllen, egal ob angezeigt
                    object[m] = "";
                } else {
                    object[m] = datenAbo.arr[m];
                }
            }
            model.addRow(object);
        }
    }

    public Datum getDatumForObject(String datum) {
        Datum tmp = new Datum(0);
        if (!datum.equals("")) {
            try {
                tmp.setTime(new SimpleDateFormat("dd.MM.yyyy").parse(datum).getTime());
            } catch (ParseException ex) {
            }
        }
        return tmp;
    }

    public ArrayList<String> getPfade() {
        // liefert eine Array mit allen Pfaden
        ArrayList<String> pfade = new ArrayList<>();
        for (DatenAbo abo : this) {
            String s = abo.arr[DatenAbo.ABO_ZIELPFAD_NR];
            if (!pfade.contains(s)) {
                pfade.add(abo.arr[DatenAbo.ABO_ZIELPFAD_NR]);
            }
        }
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        Collections.sort(pfade, sorter);
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
                if (!Filter.laengePruefen(((DatenAbo) film.abo).mindestdauerMinuten, film.dauerL)) {
                    return null;
                }
            }
            return (DatenAbo) film.abo;
        }
    }

    public void setAboFuerFilm(ListeFilme listeFilme, boolean aboLoeschen) {
        // hier wird tatsächlich für jeden Film die Liste der Abos durchsucht
        // braucht länger
        DatenFilm datenFilm;
        ListIterator<DatenFilm> iteratorFilm = listeFilme.listIterator();
        DatenAbo datenAbo;
        ListIterator<DatenAbo> iteratorAbo;
        if (this.size() == 0 && aboLoeschen) {
            while (iteratorFilm.hasNext()) {
                // für jeden Film Abo löschen
                datenFilm = iteratorFilm.next();
                datenFilm.arr[DatenFilm.FILM_ABO_NAME_NR] = "";
                datenFilm.abo = null;
            }
        } else {
            while (iteratorFilm.hasNext()) {
                // für jeden Film
                datenFilm = iteratorFilm.next();
                datenFilm.arr[DatenFilm.FILM_ABO_NAME_NR] = "";
                datenFilm.abo = null;
                iteratorAbo = this.listIterator();
                while (iteratorAbo.hasNext()) {
                    // jedes Abo prüfen
                    datenAbo = iteratorAbo.next();
                    if (datenAbo.arr[DatenAbo.ABO_TITEL_NR].isEmpty()) {
                        titel = LEER;
                    } else {
                        titel = Filter.isPattern(datenAbo.arr[DatenAbo.ABO_TITEL_NR])
                                ? new String[]{datenAbo.arr[DatenAbo.ABO_TITEL_NR].toLowerCase()} : datenAbo.arr[DatenAbo.ABO_TITEL_NR].toLowerCase().split(",");
                    }
                    if (datenAbo.arr[DatenAbo.ABO_THEMA_TITEL_NR].isEmpty()) {
                        thema = LEER;
                    } else {
                        thema = Filter.isPattern(datenAbo.arr[DatenAbo.ABO_THEMA_TITEL_NR])
                                ? new String[]{datenAbo.arr[DatenAbo.ABO_THEMA_TITEL_NR].toLowerCase()} : datenAbo.arr[DatenAbo.ABO_THEMA_TITEL_NR].toLowerCase().split(",");
                    }
                    if (datenAbo.arr[DatenAbo.ABO_IRGENDWO_NR].isEmpty()) {
                        irgendwo = LEER;
                    } else {
                        irgendwo = Filter.isPattern(datenAbo.arr[DatenAbo.ABO_IRGENDWO_NR])
                                ? new String[]{datenAbo.arr[DatenAbo.ABO_IRGENDWO_NR].toLowerCase()} : datenAbo.arr[DatenAbo.ABO_IRGENDWO_NR].toLowerCase().split(",");
                    }
                    if (Filter.filterAufFilmPruefen(datenAbo.arr[DatenAbo.ABO_SENDER_NR], datenAbo.arr[DatenAbo.ABO_THEMA_NR],
                            titel,
                            thema,
                            irgendwo,
                            datenAbo.mindestdauerMinuten,
                            datenFilm, false)) {
                        // das Abo im Film eintragen
                        // und noch die Filmlänge prüfen
                        if (!Filter.laengePruefen(datenAbo.mindestdauerMinuten, datenFilm.dauerL)) {
                            // dann ist der Film zu kurz
                            datenFilm.arr[DatenFilm.FILM_ABO_NAME_NR] = datenAbo.arr[DatenAbo.ABO_NAME_NR] + " [zu kurz]";
                        } else {
                            datenFilm.arr[DatenFilm.FILM_ABO_NAME_NR] = datenAbo.arr[DatenAbo.ABO_NAME_NR];
                            datenFilm.abo = datenAbo;
                        }
                        // und nichts wie weiter
                        break;
                    }
                }
            }
        }
    }
}
