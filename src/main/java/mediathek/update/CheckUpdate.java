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
package mediathek.update;

import mSearch.tool.Duration;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialog.DialogNewSet;
import mediathek.tool.GuiFunktionenProgramme;

import javax.swing.*;
import java.text.SimpleDateFormat;
import java.util.Date;

public class CheckUpdate extends Thread {
    private final Daten daten;
    private final JFrame parent;

    public CheckUpdate(JFrame parent, Daten daten) {
        this.daten = daten;
        this.parent = parent;

        Duration.staticPing("CheckUpdate");
        setName("CheckUpdate Thread");
    }

    public void run() {
        try {
            final MediathekGui gui = daten.getMediathekGui();
            if (gui != null)
                SwingUtilities.invokeLater(() -> gui.enableUpdateMenuItem(false));

            searchForProgramUpdate();

            checkForPsetUpdates();

            if (gui != null)
                SwingUtilities.invokeLater(() -> gui.enableUpdateMenuItem(true));
        } catch (Exception ex) {
            Log.errorLog(794612801, ex);
        }
    }

    private void searchForProgramUpdate() {
        final ProgrammUpdateSuchen pgrUpdate = new ProgrammUpdateSuchen();
        if (pgrUpdate.checkVersion(false /* bei aktuell anzeigen */, true /* Hinweis */, false /* hinweiseAlleAnzeigen */)) {
            Listener.notify(Listener.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, CheckUpdate.class.getSimpleName());
        }
    }

    private void checkForPsetUpdates() {
        try {
            SwingUtilities.invokeLater(() -> {
                ListePset listePsetStandard = ListePsetVorlagen.getStandarset(parent, daten, false /*replaceMuster*/);
                String version = MVConfig.get(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET);
                if (listePsetStandard != null) {
                    if (!Daten.listePset.isEmpty()) {
                        // ansonsten ist die Liste leer und dann gibts immer was
                        if (listePsetStandard.version.isEmpty()) {
                            // dann hat das Laden der aktuellen Standardversion nicht geklappt
                            return;
                        }
                        if (version.equals(listePsetStandard.version)) {
                            // dann passt alles
                            return;
                        } else {
                            DialogNewSet dialogNewSet = new DialogNewSet(parent);
                            dialogNewSet.setVisible(true);
                            if (!dialogNewSet.ok) {
                                SysMsg.sysMsg("Setanlegen: Abbruch");
                                if (!dialogNewSet.morgen) {
                                    // dann auch die Versionsnummer aktualisieren
                                    SysMsg.sysMsg("Setanlegen: Nicht wieder nachfragen");
                                    MVConfig.add(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET, listePsetStandard.version);
                                }
                                SysMsg.sysMsg("==========================================");
                                // dann halt nicht
                                return;
                            }
                        }
                    }

                    //========================================
                    // gibt keine Sets oder aktualisieren
                    // damit die Variablen ersetzt werden
                    ListePset.progMusterErsetzen(parent, listePsetStandard);

                    MVConfig.add(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET, listePsetStandard.version);
                    // die Zielpafade anpassen
                    ListePset listePsetOrgSpeichern = Daten.listePset.getListeSpeichern();
                    if (!listePsetOrgSpeichern.isEmpty()) {
                        for (DatenPset psNew : listePsetStandard.getListeSpeichern()) {
                            psNew.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_ZIEL_PFAD];
                            psNew.arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_THEMA_ANLEGEN];
                            psNew.arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_LAENGE_BESCHRAENKEN];
                            psNew.arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN];
                            psNew.arr[DatenPset.PROGRAMMSET_MAX_LAENGE] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_MAX_LAENGE];
                            psNew.arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD] = listePsetOrgSpeichern.get(0).arr[DatenPset.PROGRAMMSET_MAX_LAENGE_FIELD];
                        }
                    }
                    if (!Daten.listePset.isEmpty()) {
                        // wenn leer, dann gibts immer die neuen und die sind dann auch aktiv
                        for (DatenPset psNew : listePsetStandard) {
                            // die bestehenden Sets sollen nicht gestört werden
                            psNew.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN] = Boolean.FALSE.toString();
                            psNew.arr[DatenPset.PROGRAMMSET_IST_ABO] = Boolean.FALSE.toString();
                            psNew.arr[DatenPset.PROGRAMMSET_IST_BUTTON] = Boolean.FALSE.toString();
                            psNew.arr[DatenPset.PROGRAMMSET_IST_SPEICHERN] = Boolean.FALSE.toString();
                        }
                        // damit man sie auch findet :)
                        String date = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
                        listePsetStandard.forEach((psNew) -> psNew.arr[DatenPset.PROGRAMMSET_NAME] = psNew.arr[DatenPset.PROGRAMMSET_NAME] + ", neu: " + date);
                    }
                    GuiFunktionenProgramme.addSetVorlagen(daten.getMediathekGui(), daten, listePsetStandard, true /*auto*/, true /*setVersion*/); // damit auch AddOns geladen werden
                    SysMsg.sysMsg("Setanlegen: OK");
                    SysMsg.sysMsg("==========================================");
                }
            });
        } catch (Exception ignored) {
        }
    }

}
