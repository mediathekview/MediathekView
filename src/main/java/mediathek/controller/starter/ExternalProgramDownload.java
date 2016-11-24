/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.starter;

import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.controller.starter.DirectHttpDownload.HttpDownloadState;
import mediathek.daten.DatenDownload;
import mediathek.gui.dialog.DialogContinueDownload;
import mediathek.gui.dialog.MeldungDownloadfehler;
import mediathek.tool.MVInfoFile;
import mediathek.tool.MVSubtitle;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import static mediathek.controller.starter.StarterClass.*;

/**
 * Download files via an external program.
 */
public class ExternalProgramDownload extends Thread
{

    private Daten daten;
    private DatenDownload datenDownload;
    private Start start;
    private File file;
    private String exMessage = "";
    private boolean retAbbrechen;
    private boolean dialogAbbrechenIsVis;
    private HttpDownloadState state = HttpDownloadState.DOWNLOAD;

    public ExternalProgramDownload(Daten daten, DatenDownload d)
    {
        super();
        setName("PROGRAMM DL THREAD: " + d.arr[DatenDownload.DOWNLOAD_TITEL]);

        this.daten = daten;
        datenDownload = d;
        start = datenDownload.start;
        start.status = Start.STATUS_RUN;
        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        notifyStartEvent(datenDownload);
        try
        {
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI]))
            {
                MVInfoFile.writeInfoFile(datenDownload);
            }
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE]))
            {
                MVSubtitle.writeSubtitle(datenDownload);
            }

            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
        } catch (IOException ignored)
        {
        } catch (Exception ex)
        {
            Log.errorLog(469365281, ex);
        }
    }

    @Override
    public synchronized void run()
    {
        long filesize = -1;
        final int stat_start = 0;
        final int stat_laufen = 1;
        final int stat_restart = 3;
        final int stat_pruefen = 4;
        // ab hier ist schluss
        final int stat_fertig_ok = 10;
        final int stat_fertig_fehler = 11;
        final int stat_ende = 99;
        int stat = stat_start;
        try
        {
            if (!cancelDownload())
            {
                while (stat < stat_ende)
                {
                    switch (stat)
                    {
                        case stat_start:
                            // versuch das Programm zu Starten
                            if (starten())
                            {
                                if (datenDownload.isDownloadManager())
                                {
                                    stat = stat_fertig_ok;
                                } else
                                {
                                    stat = stat_laufen;
                                }
                            } else
                            {
                                stat = stat_restart;
                            }
                            break;
                        case stat_laufen:
                            //hier läuft der Download bis zum Abbruch oder Ende
                            try
                            {
                                if (start.stoppen)
                                {
                                    stat = stat_fertig_ok;
                                    if (start.process != null)
                                    {
                                        start.process.destroy();
                                    }
                                } else
                                {
                                    int exitV = start.process.exitValue();
                                    if (exitV != 0)
                                    {
                                        stat = stat_restart;
                                    } else
                                    {
                                        stat = stat_pruefen;
                                    }
                                }
                            } catch (Exception ex)
                            {
                                try
                                {
                                    this.wait(2000);
                                } catch (InterruptedException ignored)
                                {
                                }
                            }
                            break;
                        case stat_restart:
                            if (!datenDownload.isRestart())
                            {
                                // dann wars das
                                stat = stat_fertig_fehler;
                            } else if (filesize == -1)
                            {
                                //noch nichts geladen
                                deleteIfEmpty(file);
                                if (file.exists())
                                {
                                    // dann bestehende Datei weitermachen
                                    filesize = file.length();
                                    stat = stat_start;
                                } else // counter prüfen und bei einem Maxwert cancelDownload, sonst endlos
                                    if (start.startcounter < Start.STARTCOUNTER_MAX)
                                    {
                                        // dann nochmal von vorne
                                        stat = stat_start;
                                    } else
                                    {
                                        // dann wars das
                                        stat = stat_fertig_fehler;
                                    }
                            } else //jetzt muss das File wachsen, sonst kein Restart
                                if (!file.exists())
                                {
                                    // dann wars das
                                    stat = stat_fertig_fehler;
                                } else if (file.length() > filesize)
                                {
                                    //nur weitermachen wenn die Datei tasächlich wächst
                                    filesize = file.length();
                                    stat = stat_start;
                                } else
                                {
                                    // dann wars das
                                    stat = stat_fertig_fehler;
                                }
                            break;
                        case stat_pruefen:
                            if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON || datenDownload.isDownloadManager())
                            {
                                //für die direkten Starts mit dem Button und die remote downloads wars das dann
                                stat = stat_fertig_ok;
                            } else if (pruefen(daten, datenDownload, start))
                            {
                                //fertig und OK
                                stat = stat_fertig_ok;
                            } else
                            {
                                //fertig und fehlerhaft
                                stat = stat_fertig_fehler;
                            }
                            break;
                        case stat_fertig_fehler:
                            start.status = Start.STATUS_ERR;
                            stat = stat_ende;
                            break;
                        case stat_fertig_ok:
                            start.status = Start.STATUS_FERTIG;
                            stat = stat_ende;
                            break;
                    }
                }
            }
        } catch (Exception ex)
        {
            exMessage = ex.getLocalizedMessage();
            Log.errorLog(395623710, ex);
            SwingUtilities.invokeLater(() ->
            {
                if (!Daten.isAuto())
                {
                    new MeldungDownloadfehler(Daten.getInstance().getMediathekGui(), exMessage, datenDownload).setVisible(true);
                }
            });
        }
        finalizeDownload(datenDownload, start, state);
    }

    private boolean starten()
    {
        boolean ret = false;
        // die Reihenfolge: startcounter - startmeldung ist wichtig!
        start.startcounter++;
        startmeldung(datenDownload, start);
        RuntimeExec runtimeExec = new RuntimeExec(datenDownload.mVFilmSize, datenDownload.start,
                datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF], datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        start.process = runtimeExec.exec(true /*log*/);
        if (start.process != null)
        {
            ret = true;
        }
        return ret;
    }

    private boolean cancelDownload()
    {
        if (datenDownload.isDownloadManager())
        {
            // da kümmert sich ein anderes Programm darum
            return false;
        }
        if (!file.exists())
        {
            // dann ist alles OK
            return false;
        }
        if (Daten.isAuto())
        {
            // dann mit gleichem Namen und Datei vorher löschen
            try
            {
                Files.deleteIfExists(file.toPath());
                file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            } catch (IOException ex)
            {
                // kann nicht gelöscht werden, evtl. klappt ja das Überschreiben
                Log.errorLog(795623145, ex, "file exists: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            }
            return false; //auf keinen Fall den Dialog starten :)
        }

        dialogAbbrechenIsVis = true;
        retAbbrechen = true;
        if (SwingUtilities.isEventDispatchThread())
        {
            retAbbrechen = abbrechen_();
        } else
        {
            SwingUtilities.invokeLater(() ->
            {
                retAbbrechen = abbrechen_();
                dialogAbbrechenIsVis = false;
            });
        }
        while (dialogAbbrechenIsVis)
        {
            try
            {
                wait(100);
            } catch (Exception ignored)
            {
            }
        }
        return retAbbrechen;
    }

    private boolean abbrechen_()
    {
        boolean result = false;
        if (file.exists())
        {
            DialogContinueDownload dialogContinueDownload = new DialogContinueDownload(Daten.getInstance().getMediathekGui(), datenDownload, false /*weiterführen*/);
            dialogContinueDownload.setVisible(true);

            switch (dialogContinueDownload.getResult())
            {
                case CANCELLED:
                    // dann wars das
                    state = DirectHttpDownload.HttpDownloadState.CANCEL;
                    result = true;
                    break;

                case CONTINUE:
                    // dann mit gleichem Namen und Datei vorher löschen
                    try
                    {
                        Files.deleteIfExists(file.toPath());
                        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                    } catch (Exception ex)
                    {
                        // kann nicht gelöscht werden, evtl. klappt ja das Überschreiben
                        Log.errorLog(945120398, ex, "file exists: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                    }
                    break;

                case RESTART_WITH_NEW_NAME:
                    if (dialogContinueDownload.isNewName())
                    {
                        // jetzt den Programmaufruf nochmal mit dem geänderten Dateinamen nochmal bauen
                        datenDownload.aufrufBauen();
                        Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
                        try
                        {
                            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
                        } catch (IOException ignored)
                        {
                        }
                        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                    }
                    break;
            }
        }
        return result;
    }
}
