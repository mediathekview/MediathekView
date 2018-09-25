package mediathek.update;

import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.daten.ListePsetVorlagen;
import mediathek.gui.dialog.DialogNewSet;
import mediathek.tool.GuiFunktionenProgramme;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

/**
 * Perform check for updates every 24 hours if program is running long enough.
 */
public class ProgramUpdateCheck implements AutoCloseable {
    private static final Logger logger = LogManager.getLogger(ProgramUpdateCheck.class);
    /**
     * 24 hour timer for repeating update checks
     */
    private final Timer updateCheckTimer;

    private final Daten daten;

    public ProgramUpdateCheck(Daten daten) {
        this.daten = daten;

        updateCheckTimer = new Timer(1000, e -> ForkJoinPool.commonPool().execute(this::performUpdateCheck));
        updateCheckTimer.setRepeats(true);
        updateCheckTimer.setDelay((int) TimeUnit.MILLISECONDS.convert(24, TimeUnit.HOURS));
    }

    private void searchForProgramUpdate() {
        var pgrUpdate = new ProgrammUpdateSuchen();
        pgrUpdate.checkVersion(false, true, false);
    }

    private void checkForPsetUpdates() {
        try {
            SwingUtilities.invokeLater(() -> {
                final var parent = MediathekGui.ui();
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
                    GuiFunktionenProgramme.addSetVorlagen(MediathekGui.ui(), daten, listePsetStandard, true /*auto*/, true /*setVersion*/); // damit auch AddOns geladen werden
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
            InetAddress serverAddr = InetAddress.getByName("res.mediathekview.de");
            if (serverAddr.isReachable(1000)) {
                //we have internet...
                SwingUtilities.invokeLater(() -> gui.enableUpdateMenuItem(false));

                searchForProgramUpdate();

                checkForPsetUpdates();
            } else
                logger.warn("Update Check: Network is not reachable.");
        }
        catch (UnknownHostException ignored) {
        }
        catch (IOException ex) {
            //do not log errors as we expect them here...
            logger.error("Update check caused exception:",ex);
        } finally {
            SwingUtilities.invokeLater(() -> gui.enableUpdateMenuItem(true));
        }
        logger.debug("performUpdateCheck finished.");
    }

    public void start() {
        logger.debug("ProgramUpdateCheck Started.");
        updateCheckTimer.start();
    }

    @Override
    public void close() {
        if (updateCheckTimer != null) {
            updateCheckTimer.stop();
        }
        logger.debug("ProgramUpdateCheck closed.");
    }
}
