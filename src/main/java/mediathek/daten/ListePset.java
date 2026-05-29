/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.daten;

import mediathek.config.MVConfig;
import mediathek.config.StandardLocations;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.tool.MessageBus;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.stream.Collectors;

public class ListePset extends ArrayList<DatenPset> {
    // Liste aller Programmsets
    public static final String MUSTER_PFAD_ZIEL = "ZIELPFAD";
    public static final String MUSTER_PFAD_VLC = "PFAD_VLC";
    public static final String MUSTER_PFAD_FFMPEG = "PFAD_FFMPEG";
    public String version = "";

    public static void progMusterErsetzen(JFrame parent, ListePset liste) {
        for (DatenPset pSet : liste) {
            progMusterErsetzen(parent, pSet);
        }

        MessageBus.getMessageBus().publishAsync(new ProgramSetChangedEvent());
    }

    private static void progMusterErsetzen(JFrame parent, DatenPset pSet) {
        pSet.setZielPfad(pSet.getZielPfad().replace(MUSTER_PFAD_ZIEL, StandardLocations.getStandardDownloadPath()));
        String vlc = "";
        String ffmpeg = "";

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
            // ffmpeg
            prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD]
                    = prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD].replaceAll(MUSTER_PFAD_FFMPEG, Matcher.quoteReplacement(ffmpeg));
            prog.arr[DatenProg.PROGRAMM_SCHALTER]
                    = prog.arr[DatenProg.PROGRAMM_SCHALTER].replaceAll(MUSTER_PFAD_FFMPEG, Matcher.quoteReplacement(ffmpeg));
        }
    }

    private static String getPfadVlc(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC).isEmpty()) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, true /* vlc */, false/*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC);
    }

    private static String getPfadFFmpeg(JFrame parent) {
        // liefert den Pfad wenn vorhanden, wenn nicht wird er in einem Dialog abgefragt
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG).isEmpty()) {
            new DialogOk(null, true, new PanelProgrammPfade(parent, false /* vlc */, true /*ffmpeg*/), "Pfade Standardprogramme").setVisible(true);
        }
        return MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG);
    }

    /**
     * Make the specified pset active as the current player. Deactivates all other psets.
     * @param pset the current pset to activate
     */
    public void activateAsPlayer(DatenPset pset) {
        forEach(set -> set.setAbspielen(false));
        pset.setAbspielen(true);
    }

    private void normalizePlaybackSelection(DatenPset datenPset) {
        if (datenPset != null && datenPset.istAbspielen()) {
            forEach(set -> set.setAbspielen(false));
        }
    }

    private void normalizePlaybackSelection(Collection<? extends DatenPset> collection) {
        DatenPset lastActive = null;
        for (DatenPset datenPset : collection) {
            if (datenPset.istAbspielen()) {
                if (lastActive != null) {
                    lastActive.setAbspielen(false);
                }
                lastActive = datenPset;
            }
        }
        normalizePlaybackSelection(lastActive);
    }

    @Override
    public boolean add(DatenPset datenPset) {
        normalizePlaybackSelection(datenPset);
        return super.add(datenPset);
    }

    @Override
    public void add(int index, DatenPset element) {
        normalizePlaybackSelection(element);
        super.add(index, element);
    }

    @Override
    public boolean addAll(Collection<? extends DatenPset> collection) {
        normalizePlaybackSelection(collection);
        return super.addAll(collection);
    }

    @Override
    public boolean addAll(int index, Collection<? extends DatenPset> collection) {
        normalizePlaybackSelection(collection);
        return super.addAll(index, collection);
    }

    @Override
    public DatenPset set(int index, DatenPset element) {
        normalizePlaybackSelection(element);
        return super.set(index, element);
    }

    public DatenPset getPsetAbspielen() {
        //liefert die Programmgruppe zum Abspielen
        for (DatenPset datenPset : this) {
            if (datenPset.istAbspielen()) {
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
            ret = get(0);
        } else {
            for (DatenPset pset : this) {
                if (pset.istAbo()) {
                    if (pset.getName().equals(name)) {
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
                        ret = get(0);
                    }
                }
            }
        }
        return ret;
    }

    public ListePset getListeSpeichern() {
        // liefert eine Liste Programmsets, die zum Speichern angelegt sind (ist meist nur eins)
        return this.stream().filter(DatenPset::istSpeichern)
                .collect(Collectors.toCollection(ListePset::new));
    }

    public ListePset getListeButton() {
        // liefert eine Liste Programmsets, die als Button angelegt sind
        return this.stream().filter(DatenPset::istButton)
                .collect(Collectors.toCollection(ListePset::new));
    }

    public ListePset getListeAbo() {
        // liefert eine Liste Programmsets, die für Abos angelegt sind (ist meist nur eins)
        return this.stream().filter(DatenPset::istAbo)
                .collect(Collectors.toCollection(ListePset::new));
    }

    public String[] getObjectDataCombo() {
        //liefert eine Liste aller Psetnamen
        String[] object;
        int i = 0;
        object = new String[this.size()];
        for (DatenPset datenPset : this) {
            object[i] = datenPset.getName();
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

        MessageBus.getMessageBus().publishAsync(new ProgramSetChangedEvent());

        return neu;
    }

    public void addPset(DatenPset datenPset) {
        add(datenPset);

        MessageBus.getMessageBus().publishAsync(new ProgramSetChangedEvent());

    }

    public boolean addPset(ListePset liste) {
        boolean ret = true;
        for (DatenPset entry : liste) {
            if (!add(entry)) {
                ret = false;
            }
        }

        if (ret) {
            MessageBus.getMessageBus().publishAsync(new ProgramSetChangedEvent());
        }

        return ret;
    }

    public TableModel createModel() {
        Object[][] object = new Object[this.size()][DatenPset.MAX_ELEM];
        int i = 0;
        for (DatenPset datenPset : this) {
            object[i] = createModelRow(datenPset);
            ++i;
        }
        return new PsetTableModel(object);
    }

    private static Object[] createModelRow(DatenPset datenPset) {
        Object[] row = Arrays.copyOf(datenPset.toArray(), DatenPset.MAX_ELEM, Object[].class);
        row[DatenPset.PROGRAMMSET_IST_ABSPIELEN] = datenPset.istAbspielen();
        row[DatenPset.PROGRAMMSET_IST_SPEICHERN] = datenPset.istSpeichern();
        return row;
    }

    private static final class PsetTableModel extends DefaultTableModel {
        private PsetTableModel(Object[][] data) {
            super(data, DatenPset.COLUMN_NAMES);
        }

        @Override
        public boolean isCellEditable(int row, int column) {
            return false;
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return switch (columnIndex) {
                case DatenPset.PROGRAMMSET_IST_ABSPIELEN,
                     DatenPset.PROGRAMMSET_IST_SPEICHERN -> Boolean.class;
                default -> String.class;
            };
        }
    }
}
