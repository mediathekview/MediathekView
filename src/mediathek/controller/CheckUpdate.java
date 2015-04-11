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
import mediathek.daten.Daten;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialog.DialogNewSet;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;

public class CheckUpdate {

    private final Daten daten;
    private final JFrame parent;

    public CheckUpdate(JFrame pparent, Daten dd) {
        daten = dd;
        parent = pparent;
    }

    public void suchen() {
        new Thread(new Pruefen()).start();
    }

    private class Pruefen implements Runnable {

        @Override
        public synchronized void run() {
            try {
                if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_UPDATE_SUCHEN))) {
                    if (!Daten.mVConfig.get(MVConfig.SYSTEM_UPDATE_DATUM).equals(new SimpleDateFormat("yyyyMMdd").format(new Date()))) {
                        final ProgrammUpdateSuchen pgrUpdate = new ProgrammUpdateSuchen();
                        if (pgrUpdate.checkVersion(daten, false /* bei aktuell anzeigen */, true /* Hinweis */, false /* hinweiseAlleAnzeigen */)) {
                            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, CheckUpdate.class.getSimpleName());
                        } else {
                            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, CheckUpdate.class.getSimpleName());
                        }

                        checkSet();

                        try {
                            this.wait(10 * 1000); // 10 Sekunden den Titel anzeigen
                        } catch (Exception ignored) {
                        }
                        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, CheckUpdate.class.getSimpleName());
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(794612801, ex);
            }
        }
    }

    private void checkSet() {
        ListePset listePsetStandard = ListePsetVorlagen.getStandarset(parent, daten, false /*replaceMuster*/);;
        String version = Daten.mVConfig.get(MVConfig.SYSTEM_VERSION_PROGRAMMSET);
        if (listePsetStandard != null) {
            if (Daten.listePset.size() > 0) {
                // ansonsten ist die Liste leer und dann gibts immer was
                if (listePsetStandard.version.isEmpty()) {
                    // dann hat das Laden der aktuellen Standardversion nicht geklappt
                    return;
                }
                if (version.equals(listePsetStandard.version)) {
                    // dann passt alles
                    return;
                } else {
                    DialogNewSet dialogNewSet = new DialogNewSet(parent, daten);
                    dialogNewSet.setVisible(true);
                    if (!dialogNewSet.ok) {
                        if (!dialogNewSet.morgen) {
                            // dann auch die Versionsnummer aktualisieren
                            Daten.mVConfig.add(MVConfig.SYSTEM_VERSION_PROGRAMMSET, listePsetStandard.version);
                        }
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
                    psNew.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_MAX_LAENGE_NR];
                }
            }
            if (!Daten.listePset.isEmpty()) {
                // wenn leer, dann gibt immer die Neuen und die sind dann auch aktiv
                for (DatenPset psNew : listePsetStandard) {
                    // die bestehenden Sets sollen nicht gestört werden
                    psNew.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.FALSE.toString();
                    psNew.arr[DatenPset.PROGRAMMSET_IST_ABO_NR] = Boolean.FALSE.toString();
                    psNew.arr[DatenPset.PROGRAMMSET_IST_BUTTON_NR] = Boolean.FALSE.toString();
                    psNew.arr[DatenPset.PROGRAMMSET_IST_SPEICHERN_NR] = Boolean.FALSE.toString();
                }
            }
            GuiFunktionenProgramme.addSetVorlagen(daten.mediathekGui, daten, listePsetStandard, true /*auto*/, true /*setVersion*/); // damit auch AddOns geladen werden
        }
    }

}
