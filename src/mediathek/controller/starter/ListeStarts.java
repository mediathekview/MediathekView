/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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

import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.TModel;

public class ListeStarts extends LinkedList<Start> {

    Daten ddaten;

    public ListeStarts(Daten d) {
        super();
        ddaten = d;
    }

    synchronized void addStart(Start start) {
        //add: Neues Element an die Liste anhängen
        if (start != null) {
            if (!contain(start)) {
                add(start);
                // gestartete Filme (originalURL des Films) auch in die History eintragen
                ddaten.history.add(start.datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL_NR]);
            }
        }
        notifyStartEvent();
    }

    synchronized void addStart(ArrayList<Start> start) {
        //add: Neues Element an die Liste anhängen
        if (start != null) {
            ArrayList<String> al = new ArrayList<String>();
            for (Start s : start) {
                if (!contain(s)) {
                    add(s);
                    // gestartete Filme auch in die History eintragen
                    al.add(s.datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL_NR]);
                }
            }
            ddaten.history.add(al.toArray(new String[]{}));
        }
        notifyStartEvent();
    }

    synchronized Start getStartOrgUrl(String orgUrl) {
        Start ret = null;
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL_NR].equals(orgUrl)) {
                ret = s;
                break;
            }
        }
        return ret;
    }

    synchronized Start getStart(String url) {
        Start ret = null;
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                ret = s;
                break;
            }
        }
        return ret;
    }

    synchronized LinkedList<Start> getStarts(int quelle) {
        LinkedList<Start> ret = new LinkedList<Start>();
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.getQuelle() == quelle || quelle == Start.QUELLE_ALLE) {
                ret.add(s);
            }
        }
        return ret;
    }

    synchronized Start urlVorziehen(String url) {
        // Starts mit der URL wird vorgezogen und startet als nächster
        Start s = null;
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                if (s.status < Start.STATUS_RUN) {
                    // sonst bringts nichts mehr
                    it.remove();
                    addFirst(s);
                }
                break;
            }
        }
        return s;
    }

    synchronized Start urlTauschen(String url) {
        // Starts mit der URL wird vorgezogen und startet als nächster
        Start s = null;
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                if (s.status < Start.STATUS_RUN) {
                    // sonst bringts nichts mehr
                    it.remove();
                    addFirst(s);
                }
                break;
            }
        }
        return s;
    }

    synchronized void buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        boolean gefunden = false;
        Iterator<Start> it = iterator();
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                if (s.status != Start.STATUS_RUN) {
                    // dann ist er fertig oder abgebrochen
                    it.remove();
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            notifyStartEvent(); // und dann bescheid geben
        }
    }

    synchronized Start getListe() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        Start ret = null;
        if (size() >= 0 && getDown() < Integer.parseInt(Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR])) {
            Start s = naechsterStart();
            if (s != null) {
                if (s.status == Start.STATUS_INIT) {
                    ret = s;
                }
            }
        }
        return ret;
    }

    synchronized void setBanbreite() {
        Start s;
        Iterator<Start> it = iterator();
        //erster Versuch, Start mit einem anderen Sender
        while (it.hasNext()) {
            s = it.next();
            if (s.mVInputStream != null) {
                s.mVInputStream.setBandbreite();
            }
        }
    }

    void delStart(ArrayList<String> url) {
        ListIterator<Start> it;
        if (url != null) {
            for (String u : url) {
                it = this.listIterator(0);
                while (it.hasNext()) {
                    Start s = it.next();
                    if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(u)) {
                        s.stoppen = true;
                        it.remove();
                        break;
                    }
                }
            }
        }
        notifyStartEvent();
    }

    void delStart(String url) {
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                s.stoppen = true;
                it.remove();
                notifyStartEvent();
                break;
            }
        }
    }

    void delAllStart() {
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            s.stoppen = true;
            it.remove();
        }
        notifyStartEvent();
    }

    void aufraeumen() {
        boolean gefunden = false;
        Iterator<Start> it = this.iterator();
        while (it.hasNext()) {
            Start start = it.next();
            if (start.status >= Start.STATUS_FERTIG) {
                it.remove();
                gefunden = true;
            }
        }
        if (gefunden) {
            notifyStartEvent();
        }
    }

    TModel getModelStarts(TModel model) {
        model.setRowCount(0);
        Object[] object;
        Start start;
        if (this.size() > 0) {
            Iterator<Start> iterator = iterator();
            int objLen = DatenDownload.MAX_ELEM + 1;
            object = new Object[objLen];
            while (iterator.hasNext()) {
                start = iterator.next();
                for (int k = 0; k < objLen; ++k) {
                    if (k < DatenDownload.MAX_ELEM) {
                        object[k] = start.datenDownload.arr[k];
                    } else {
                        if (start.datenDownload.istAbo()) {
                            object[k] = "Abo";
                        } else {
                            object[k] = "";
                        }
                    }
                }
                model.addRow(object);
            }
        }
        return model;
    }

    public static TModel getEmptyModel() {
        int max = DatenDownload.MAX_ELEM + 1;
        String[] titel = new String[max];
        for (int i = 0; i < max; ++i) {
            if (i < DatenDownload.MAX_ELEM) {
                titel[i] = DatenDownload.COLUMN_NAMES[i];
            } else {
                titel[i] = "Art";
            }
        }
        TModel model = new TModel(new Object[][]{}, titel);
        return model;
    }

    // ***********************************
    // private
    // ***********************************
    private int getDown() {
        int ret = 0;
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next();
            if (s.status == Start.STATUS_RUN) {
                ++ret;
            }
        }
        return ret;
    }

    private boolean contain(Start start) {
        boolean ret = false;
        ListIterator<Start> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR])) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    private Start naechsterStart() {
        Start s;
        Iterator<Start> it = iterator();
        //erster Versuch, Start mit einem anderen Sender
        while (it.hasNext()) {
            s = it.next();
            if (s.status == Start.STATUS_INIT) {
                if (!maxSenderLaufen(s, 1)) {
                    return s;
                }
            }
        }
        if (Konstanten.MAX_SENDER_FILME_LADEN == 1) {
            //dann wars dass
            return null;
        }
        //zweiter Versuch, Start mit einem passenden Sender
        it = iterator();
        while (it.hasNext()) {
            s = it.next();
            if (s.status == Start.STATUS_INIT) {
                //int max = s.film.arr[Konstanten.FILM_SENDER_NR].equals(Konstanten.SENDER_PODCAST) ? Konstanten.MAX_PODCAST_FILME_LADEN : Konstanten.MAX_SENDER_FILME_LADEN;
                if (!maxSenderLaufen(s, Konstanten.MAX_SENDER_FILME_LADEN)) {
                    return s;
                }
            }
        }
        return null;
    }

    private boolean maxSenderLaufen(Start s, int max) {
        //true wenn bereits die maxAnzahl pro Sender läuft
        try {
            int counter = 0;
            Start start;
            String host = getHost(s);
            Iterator<Start> it = iterator();
            while (it.hasNext()) {
                start = it.next();
                if (start.status == Start.STATUS_RUN
                        && getHost(start).equalsIgnoreCase(host)) {
                    counter++;
                    if (counter >= max) {
                        return true;
                    }
                }
            }
            return false;
        } catch (Exception ex) {
            return false;
        }
    }

    private String getHost(Start s) {
        String host = "";
        try {
            try {
                String uurl = s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR];
                // die funktion "getHost()" kann nur das Protokoll "http" ??!??
                if (uurl.startsWith("rtmpt:")) {
                    uurl = uurl.toLowerCase().replace("rtmpt:", "http:");
                }
                if (uurl.startsWith("rtmp:")) {
                    uurl = uurl.toLowerCase().replace("rtmp:", "http:");
                }
                if (uurl.startsWith("mms:")) {
                    uurl = uurl.toLowerCase().replace("mms:", "http:");
                }
                URL url = new URL(uurl);
                String tmp = url.getHost();
                if (tmp.contains(".")) {
                    host = tmp.substring(tmp.lastIndexOf("."));
                    tmp = tmp.substring(0, tmp.lastIndexOf("."));
                    if (tmp.contains(".")) {
                        host = tmp.substring(tmp.lastIndexOf(".") + 1) + host;
                    } else if (tmp.contains("/")) {
                        host = tmp.substring(tmp.lastIndexOf("/") + 1) + host;
                    } else {
                        host = "host";
                    }
                }
            } catch (Exception ex) {
                // für die Hosts bei denen das nicht klappt
                // Log.systemMeldung("getHost 1: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                host = "host";
            } finally {
                if (host == null) {
                    // Log.systemMeldung("getHost 2: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                    host = "host";
                }
                if (host.equals("")) {
                    // Log.systemMeldung("getHost 3: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                    host = "host";
                }
            }
        } catch (Exception ex) {
            // Log.systemMeldung("getHost 4: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
            host = "exception";
        }
        return host;
    }

    private void notifyStartEvent() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
    }
}
