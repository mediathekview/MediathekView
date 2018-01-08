/*
 *   MediathekView
 *   Copyright (C) 2008 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import javafx.application.Platform;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import org.controlsfx.control.Notifications;

public class MVNotification {

    public static void addNotification(DatenDownload datenDownload, boolean erfolgreich) {
        if (Daten.getInstance().getMediathekGui() == null) {
            return; // dann gibts keine GUI
        }
        final String[] m = {
            "Film:   " + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
            "Sender: " + datenDownload.arr[DatenDownload.DOWNLOAD_SENDER],
                "Größe:  " + MVFilmSize.humanReadableByteCount(datenDownload.mVFilmSize.getSize(), true)
        };
        addNew(m, erfolgreich);
    }

    private static void addNew(String[] mmeldung, boolean erfolgreich) {
        if (!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_NOTIFICATION)))
            return;

        StringBuilder meldung = new StringBuilder();
        for (String s : mmeldung) {
            meldung.append(s).append('\n');
        }

        Platform.runLater(() -> {
            Notifications msg = Notifications.create();
            if (erfolgreich)
                msg.title("Download war erfolgreich");
            else
                msg.title("Download war fehlerhaft");
            msg.text(meldung.toString());
            if (erfolgreich)
                msg.showInformation();
            else
                msg.showError();
        });
    }
}
