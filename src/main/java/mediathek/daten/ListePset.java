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

import mSearch.tool.Listener;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.TModel;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

@SuppressWarnings("serial")
public class ListePset extends LinkedList<DatenPset> {
    // Liste aller Programmsets
    public static final String MUSTER_PFAD_ZIEL = "ZIELPFAD";
    public static final String MUSTER_PFAD_VLC = "PFAD_VLC";
    public static final String MUSTER_PFAD_FLV = "PFAD_FLVSTREAMER";
    public static final String MUSTER_PFAD_FFMPEG = "PFAD_FFMPEG";
    public static final String MUSTER_PFAD_SCRIPT = "PFAD_SCRIPT";
    public String version = "";

    public DatenPset getPsetAbspielen() {
        //liefert die Programmgruppe zum Abspielen
        for (DatenPset datenPset : this) {
            if (Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN])) {
                return datenPset;
            }
        }
        return null;
    }

    public DatenPset getPsetAbo(String name) {
        // liefert mit dem Namen eines Abos die passende Programmgruppe zurück
        // wird nichts gefunden, wird die erste Programmgruppe (der Abos) genommen
        DatenPset ret = null;
        if (this.isEmpty()) {
            ret = null;
        } else if (this.size() == 1) {
            ret = this.getFirst();
        } else {
            for (DatenPset pset : this) {
                if (pset.istAbo()) {
                    if (pset.arr[DatenPset.PROGRAMMSET_NAME].equals(name)) {
                        ret = pset;
                    }
                }
            }
            if (ret == null) {
                // die erste Pset der Abos
                ListePset ps = getListeAbo();
                if (ps.size() > 0) {
                    ret = getListeAbo().getFirst();
                    if (ret == null) {
                        // dann die erste Prgruppe
                        ret = this.getFirst();
                    }
                }
            }
        }
        return ret;
    }

    public ListePset getListeSpeichern() {
        // liefert eine Liste Programmsets, die zum Speichern angelegt sind (ist meist nur eins)
        return this.stream().filter(datenPset -> Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_SPEICHERN]))
                .collect(Collectors.toCollection(ListePset::new));
    }

    public ListePset getListeButton() {
        // liefert eine Liste Programmsets, die als Button angelegt sind
        return this.stream().filter(datenPset -> Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_BUTTON]))
                .collect(Collectors.toCollection(ListePset::new));
    }

    public ListePset getListeAbo() {
        // liefert eine Liste Programmsets, die für Abos angelegt sind (ist meist nur eins)
        return this.stream().filter(datenPset -> Boolean.parseBoolean(datenPset.arr[DatenPset.PROGRAMMSET_IST_ABO]))
                .collect(Collectors.toCollection(ListePset::new));
    }

    public String[] getObjectDataCombo() {
        //liefert eine Liste aller Psetnamen
        String[] object;
        int i = 0;
        object = new String[this.size()];
        for (DatenPset datenPset : this) {
            object[i] = datenPset.arr[DatenPset.PROGRAMMSET_NAME];
            ++i;
        }
        return object;
    }

    public int auf(int idx, boolean auf) {
        DatenPset prog = this.remove(idx);
        int neu = idx;
        if (auf) {
            if (neu > 0) {
                --neu;
            }
        } else if (neu < this.size()) {
            ++neu;
        }
        this.add(neu, prog);
        Listener.notify(Listener.EREIGNIS_LISTE_PSET, ListePset.class.getSimpleName());
        return neu;
    }

    public boolean addPset(DatenPset datenPset) {
        boolean abspielen = false;
        for (DatenPset datenPset1 : this) {
            if (Boolean.parseBoolean(datenPset1.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN])) {
                abspielen = true;
                break;
            }
        }
        if (abspielen) {
            datenPset.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN] = Boolean.FALSE.toString();
        }
        boolean ret = add(datenPset);
        Listener.notify(Listener.EREIGNIS_LISTE_PSET, ListePset.class.getSimpleName());
        return ret;
    }

    public boolean addPset(ListePset liste) {
        boolean ret = true;
        for (DatenPset entry : liste) {
            if (!addPset(entry)) {
                ret = false;
            }
        }
        Listener.notify(Listener.EREIGNIS_LISTE_PSET, ListePset.class.getSimpleName());
        return ret;
    }

    public static boolean progMusterErsetzen(JFrame parent, ListePset liste) {
        boolean ret = true;
        for (DatenPset pSet : liste) {
            if (!progMusterErsetzen(parent, pSet)) {
                ret = false;
            }
        }
        Listener.notify(Listener.EREIGNIS_LISTE_PSET, ListePset.class.getSimpleName());
        return ret;
    }

    private static boolean progMusterErsetzen(JFrame parent, DatenPset pSet) {
        pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD].replace(MUSTER_PFAD_ZIEL, GuiFunktionen.getStandardDownloadPath());
        String vlc = "";
        String flvstreamer = "";
        String ffmpeg = "";
        String skript = GuiFunktionenProgramme.getPfadScript();
        // damit nur die Variablen abgefragt werden, die auch verwendet werden
        for (int p = 0; p < pSet.getListeProg().size(); ++p) {
            DatenProg prog = pSet.getProg(p);
            if (prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].contains(MUSTER_PFAD_VLC) || prog.arr[DatenProg.PROGRAMM_SCHALTER].contains(MUSTER_PFAD_VLC)) {
                vlc = getPfadVlc(parent);
                break;
            }
        }
        for (int p = 0; p < pSet.getListeProg().size(); ++p) {
            DatenProg prog = pSet.getProg(p);
            if (prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].contains(MUSTER_PFAD_FLV) || prog.arr[DatenProg.PROGRAMM_SCHALTER].contains(MUSTER_PFAD_FLV)) {
                flvstreamer = getPfadFlv(parent);
                break;
            }
        }
        for (int p = 0; p < pSet.getListeProg().size(); ++p) {
            DatenProg prog = pSet.getProg(p);
            if (prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].contains(MUSTER_PFAD_FFMPEG) || prog.arr[DatenProg.PROGRAMM_SCHALTER].contains(MUSTER_PFAD_FFMPEG)) {
                ffmpeg = getPfadFFmpeg(parent);
                break;
            }
        }
        for (int p = 0; p < pSet.getListeProg().size(); ++p) {
            DatenProg prog = pSet.getProg(p);
            // VLC
            prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]
                    = prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].replaceAll(MUSTER_PFAD_VLC, Matcher.quoteReplacement(vlc));
            prog.arr[DatenProg.PROGRAMM_SCHALTER]
                    = prog.arr[DatenProg.PROGRAMM_SCHALTER].replaceAll(MUSTER_PFAD_VLC, Matcher.quoteReplacement(vlc));
            // flvstreamer
            prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]
                    = prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].replaceAll(MUSTER_PFAD_FLV, Matcher.quoteReplacement(flvstreamer));
            prog.arr[DatenProg.PROGRAMM_SCHALTER]
                    = prog.arr[DatenProg.PROGRAMM_SCHALTER].replaceAll(MUSTER_PFAD_FLV, Matcher.quoteReplacement(flvstreamer));
            // ffmpeg
            prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]
                    = prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].replaceAll(MUSTER_PFAD_FFMPEG, Matcher.quoteReplacement(ffmpeg));
            prog.arr[DatenProg.PROGRAMM_SCHALTER]
                    = prog.arr[DatenProg.PROGRAMM_SCHALTER].replaceAll(MUSTER_PFAD_FFMPEG, Matcher.quoteReplacement(ffmpeg));
            // script
            prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]
                    = prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].replaceAll(MUSTER_PFAD_SCRIPT, Matcher.quoteReplacement(skript));
            prog.arr[DatenProg.PROGRAMM_SCHALTER]
                    = prog.arr[DatenProg.PROGRAMM_SCHALTER].replaceAll(MUSTER_PFAD_SCRIPT, Matcher.quoteReplacement(skript));
        }
        return true;
    }

    private static String getPfadVlc(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, true /* vlc */, false /* flvstreamer */, false/*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC);
    }

    private static String getPfadFlv(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, false /* vlc */, true /* flvstreamer */, false/*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER);
    }

    private static String getPfadFFmpeg(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG).equals("")) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, false /* vlc */, false /* flvstreamer */, true /*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG);
    }

    public TModel getModel() {
        TModel model;
        Object[][] object;
        DatenPset datenPset;
        int i = 0;
        if (this.size() > 0) {
            Iterator<DatenPset> iterator = this.iterator();
            object = new Object[this.size()][DatenPset.MAX_ELEM];
            while (iterator.hasNext()) {
                datenPset = iterator.next();
                object[i] = datenPset.arr;
                ++i;
            }
            model = new TModel(object, DatenPset.COLUMN_NAMES);
        } else {
            model = new TModel(new Object[0][DatenPset.MAX_ELEM], DatenPset.COLUMN_NAMES);
        }
        return model;
    }

    public ArrayList<String> getListProg() {
        return this.stream().map(DatenPset::toString).collect(Collectors.toCollection(ArrayList::new));
    }
}
