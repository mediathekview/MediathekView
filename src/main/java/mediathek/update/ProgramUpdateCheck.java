package mediathek.update;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialog.DialogNewSet;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.NetUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Perform check for updates every 24 hours if program is running long enough.
 */
public class ProgramUpdateCheck implements AutoCloseable {
    private static final Logger logger = LogManager.getLogger(ProgramUpdateCheck.class);
    private final Daten daten;
    private ScheduledFuture<?> actionFuture;

    public ProgramUpdateCheck(Daten daten) {
        this.daten = daten;
    }

    private void searchForProgramUpdate() {
        var pgrUpdate = new ProgrammUpdateSuchen();
        pgrUpdate.checkVersion(false, true, false, true);
    }

    private void checkForPsetUpdates() {
        try {
            SwingUtilities.invokeLater(() -> {
                final var parent = MediathekGui.ui();
                ListePset listePsetStandard = ListePsetVorlagen.getStandarset(parent, false);
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
                                logger.info("Setanlegen: Abbruch");
                                if (!dialogNewSet.morgen) {
                                    // dann auch die Versionsnummer aktualisieren
                                    logger.info("Setanlegen: Nicht wieder nachfragen");
                                    MVConfig.add(MVConfig.Configs.SYSTEM_VERSION_PROGRAMMSET, listePsetStandard.version);
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
                    GuiFunktionenProgramme.addSetVorlagen(MediathekGui.ui(), daten, listePsetStandard, true); // damit auch AddOns geladen werden
                    logger.info("Setanlegen: OK");
                    logger.info("==========================================");
                }
            });
        } catch (Exception ignored) {
        }
    }

    private void performUpdateCheck() {
        logger.debug("performUpdateCheck started.");
        var gui = MediathekGui.ui();
        try {
            //first check if network is available...
            if (NetUtils.isReachable("res.mediathekview.de",1000)) {
                //we have internet...
                SwingUtilities.invokeLater(() -> gui.enableUpdateMenuItem(false));

                var externalUpdateCheck = System.getProperty(Konstanten.EXTERNAL_UPDATE_PROPERTY);
                if (externalUpdateCheck == null || !externalUpdateCheck.equalsIgnoreCase("true")) {
                    searchForProgramUpdate();
                }
                else {
                    logger.info("External Update Mechanism in use -> skip program update check");
                }

                checkForPsetUpdates();
            } else
                logger.warn("Update Check: Network is not reachable.");
        }
        finally {
            SwingUtilities.invokeLater(() -> gui.enableUpdateMenuItem(true));
        }
        logger.debug("performUpdateCheck finished.");
    }

    public void start() {
        logger.debug("ProgramUpdateCheck Started.");
        actionFuture = daten.getTimerPool().scheduleWithFixedDelay(() -> SwingUtilities.invokeLater(this::performUpdateCheck), 60L, TimeUnit.SECONDS.convert(24L, TimeUnit.HOURS), TimeUnit.SECONDS);
    }

    @Override
    public void close() {
        if (actionFuture != null) {
            actionFuture.cancel(true);
        }
        logger.debug("ProgramUpdateCheck closed.");
    }
}
