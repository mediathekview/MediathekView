/*
 *  MediathekView
 *  Copyright (C) 2008 W. Xaver
 *  W.Xaver[at]googlemail.com
 *  http://zdfmediathk.sourceforge.net/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller;

import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import mSearch.tool.MSLog;
import mediathek.daten.Daten;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialog.DialogNewSet;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVFunctionSys;

public class CheckUpdate {

    private final Daten daten;
    private final JFrame parent;
    private static boolean run = false;

    public CheckUpdate(JFrame pparent, Daten dd) {
        daten = dd;
        parent = pparent;
    }

    public void checkProgUpdate() {
        new Thread(new ProgPruefen()).start();
    }

    private class ProgPruefen implements Runnable {

        @Override
        public synchronized void run() {
            // Sets auf Update prüfen
            try {
                if (SwingUtilities.isEventDispatchThread()) {
                    prog();
                } else {
                    SwingUtilities.invokeLater(CheckUpdate.this::prog);
                }
            } catch (Exception ex) {
                MSLog.fehlerMeldung(794510101, ex);
            }
        }
    }

    private void prog() {
        try {
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_UPDATE_SUCHEN))) {
                if (!Daten.mVConfig.get(MVConfig.SYSTEM_BUILD_NR).equals(MVFunctionSys.getBuildNr())
                        || !Daten.mVConfig.get(MVConfig.SYSTEM_UPDATE_DATUM).equals(new SimpleDateFormat("yyyyMMdd").format(new Date()))) {
                    // damit geänderte Sets gleich gemeldet werden und nicht erst morgen
                    final ProgrammUpdateSuchen pgrUpdate = new ProgrammUpdateSuchen();
                    if (pgrUpdate.checkVersion(false /* bei aktuell anzeigen */, true /* Hinweis */, false /* hinweiseAlleAnzeigen */)) {
                        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, CheckUpdate.class.getSimpleName());
                    } else {
                        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, CheckUpdate.class.getSimpleName());
                    }

                    //==============================================
                    // Sets auf Update prüfen
                    set();

                    try {
                        this.wait(10 * 1000); // 10 Sekunden den Titel anzeigen
                    } catch (Exception ignored) {
                    }
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, CheckUpdate.class.getSimpleName());
                }
            }
        } catch (Exception ex) {
            MSLog.fehlerMeldung(794612801, ex);
        }
    }

    public void checkSet() {
        new Thread(new SetPruefen()).start();
    }

    private class SetPruefen implements Runnable {

        @Override
        public synchronized void run() {
            // Sets auf Update prüfen
            try {
                if (SwingUtilities.isEventDispatchThread()) {
                    set();
                } else {
                    SwingUtilities.invokeLater(CheckUpdate.this::set);
                }
            } catch (Exception ex) {
                MSLog.fehlerMeldung(794510101, ex);
            }
        }
    }

    private void set() {
        if (run) {
            return;// nur einmal laufen
        }
        run = true;
        ListePset listePsetStandard = ListePsetVorlagen.getStandarset(parent, daten, false /*replaceMuster*/);
        String version = Daten.mVConfig.get(MVConfig.SYSTEM_VERSION_PROGRAMMSET);
        if (listePsetStandard != null) {
            if (Daten.listePset.size() > 0) {
                // ansonsten ist die Liste leer und dann gibts immer was
                if (listePsetStandard.version.isEmpty()) {
                    // dann hat das Laden der aktuellen Standardversion nicht geklappt
                    return;
                }
                if (/*!Daten.delSets &&*/version.equals(listePsetStandard.version)) {
                    // dann passt alles
                    return;
                } else {
                    DialogNewSet dialogNewSet = new DialogNewSet(parent);
                    dialogNewSet.setVisible(true);
                    if (!dialogNewSet.ok) {
                        Log.systemMeldung("Setanlegen: Abbruch");
                        if (!dialogNewSet.morgen) {
                            // dann auch die Versionsnummer aktualisieren
                            Log.systemMeldung("Setanlegen: Nicht wieder nachfragen");
                            Daten.mVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, listePsetStandard.version);
                        }
                        Log.systemMeldung("==========================================");
                        // dann halt nicht
                        return;
                    }
                }
            }

            //========================================
            // gibt keine Sets oder aktualisieren
            // damit die Variablen ersetzt werden
            ListePset.progMusterErsetzen(parent, listePsetStandard);

            Daten.mVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, listePsetStandard.version);
            // die Zielpafade anpassen
            ListePset listePsetOrgSpeichern = Daten.listePset.getListeSpeichern();
            if (listePsetOrgSpeichern.size() > 0) {
                for (DatenPset psNew : listePsetStandard.getListeSpeichern()) {
                    psNew.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR];
                    psNew.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN_NR];
                    psNew.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN_NR];
                    psNew.arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN_NR];
                    psNew.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR];
                    psNew.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD_NR];
                }
            }
            if (!Daten.listePset.isEmpty()) {
                // wenn leer, dann gibts immer die neuen und die sind dann auch aktiv
                for (DatenPset psNew : listePsetStandard) {
                    // die bestehenden Sets sollen nicht gestört werden
                    psNew.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.FALSE.toString();
                    psNew.arr[DatenPset.PROGRAMMSET_IST_ABO_NR] = Boolean.FALSE.toString();
                    psNew.arr[DatenPset.PROGRAMMSET_IST_BUTTON_NR] = Boolean.FALSE.toString();
                    psNew.arr[DatenPset.PROGRAMMSET_IST_SPEICHERN_NR] = Boolean.FALSE.toString();
                }
                // damit man sie auch findet :)
                String date = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
                for (DatenPset psNew : listePsetStandard) {
                    psNew.arr[DatenPset.PROGRAMMSET_NAME_NR] = psNew.arr[DatenPset.PROGRAMMSET_NAME_NR] + ", neu: " + date;
                }
            }
            GuiFunktionenProgramme.addSetVorlagen(daten.mediathekGui, daten, listePsetStandard, true /*auto*/, true /*setVersion*/); // damit auch AddOns geladen werden
            Log.systemMeldung("Setanlegen: OK");
            Log.systemMeldung("==========================================");
        }
    }

}
