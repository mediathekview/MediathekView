/*
 * MediathekView Copyright (C) 2008 W. Xaver W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program. If
 * not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller;

import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;

import mSearch.filmlisten.DatenFilmlisteUrl;
import mSearch.tool.Duration;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.ReplaceList;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenMediaPath;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListeBlacklist;

public class IoXmlLesen {

  public static boolean datenLesen(Path xmlFilePath) {
    Daten daten = Daten.getInstance();
    Duration.counterStart("Konfig lesen");
    boolean ret = false;
    if (Files.exists(xmlFilePath)) {
      int event;
      XMLInputFactory inFactory = XMLInputFactory.newInstance();
      inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
      DatenPset datenPset = null;
      try (InputStreamReader in =
          new InputStreamReader(Files.newInputStream(xmlFilePath), StandardCharsets.UTF_8)) {
        // XMLStreamReader parser = inFactory.createXMLStreamReader(new BufferedReader(in, 25_000));
        XMLStreamReader parser = inFactory.createXMLStreamReader(in);
        while (parser.hasNext()) {
          event = parser.next();
          if (event == XMLStreamConstants.START_ELEMENT) {
            // String t = parser.getLocalName();
            switch (parser.getLocalName()) {
              case MVConfig.SYSTEM:
                // System
                getConfig(parser, MVConfig.SYSTEM);
                break;
              case DatenPset.TAG:
                // Programmgruppen
                datenPset = new DatenPset();
                if (get(parser, DatenPset.TAG, DatenPset.XML_NAMES, datenPset.arr)) {
                  Daten.listePset.add(datenPset);
                }
                break;
              case DatenProg.TAG:
                DatenProg datenProg = new DatenProg();
                if (get(parser, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.arr)) {
                  if (datenPset != null) {
                    datenPset.addProg(datenProg);
                  }
                }
                // ende Programgruppen
                break;
              case ReplaceList.REPLACELIST:
                // Ersetzungstabelle
                String[] sa = new String[ReplaceList.MAX_ELEM];
                if (get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)) {
                  ReplaceList.list.add(sa);
                }
                break;
              case DatenAbo.TAG:
                // Abo
                DatenAbo datenAbo = new DatenAbo();
                if (get(parser, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr)) {
                  daten.getListeAbo().addAbo(datenAbo);
                }
                break;
              case DatenDownload.TAG:
                // Downloads
                DatenDownload d = new DatenDownload();
                if (get(parser, DatenDownload.TAG, DatenDownload.XML_NAMES, d.arr)) {
                  d.init();
                  daten.getListeDownloads().add(d);
                }
                break;
              case DatenBlacklist.TAG:
                // Blacklist
                DatenBlacklist datenBlacklist = new DatenBlacklist();
                if (get(parser, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, datenBlacklist.arr)) {
                  daten.getListeBlacklist().addWithoutNotification(datenBlacklist);
                }
                break;
              case DatenMediaPath.TAG:
                // Blacklist
                DatenMediaPath mp = new DatenMediaPath();
                if (get(parser, DatenMediaPath.TAG, DatenMediaPath.XML_NAMES, mp.arr)) {
                  daten.getListeMediaPath().add(mp);
                }
                break;
              case DatenFilmlisteUrl.FILM_UPDATE_SERVER:
                // Urls Filmlisten
                DatenFilmlisteUrl datenFilmlisteUrl = new DatenFilmlisteUrl();
                if (get(parser, DatenFilmlisteUrl.FILM_UPDATE_SERVER,
                    DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenFilmlisteUrl.arr)) {
                  switch (datenFilmlisteUrl.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR]) {
                    case DatenFilmlisteUrl.SERVER_ART_AKT:
                      daten.getFilmeLaden().getDownloadUrlsFilmlisten_akt()
                          .addWithCheck(datenFilmlisteUrl);
                      break;
                    case DatenFilmlisteUrl.SERVER_ART_DIFF:
                      daten.getFilmeLaden().getDownloadUrlsFilmlisten_diff()
                          .addWithCheck(datenFilmlisteUrl);
                      break;
                  }
                }
                break;
            }
          }
        }
        parser.close();
        ret = true;
      } catch (Exception ex) {
        ret = false;
        Log.errorLog(392840096, ex);
      }
      daten.getListeDownloads().listeNummerieren();
      daten.getListeAbo().sort();
      // ListeFilmUpdateServer aufbauen
      daten.getFilmeLaden().getDownloadUrlsFilmlisten_akt().sort();
      daten.getFilmeLaden().getDownloadUrlsFilmlisten_diff().sort();
      MVConfig.loadSystemParameter();
    }

    Duration.counterStop("Konfig lesen");
    return ret;
  }

  public static boolean einstellungenExistieren() {
    Path xmlFilePath = Daten.getMediathekXmlFilePath();
    return Files.exists(xmlFilePath);
  }

  public static int[] importAboBlacklist(String datei, boolean abo, boolean black,
      boolean replace) {
    int[] found = new int[] {0, 0, 0};
    Daten daten = Daten.getInstance();
    try {
      int event;
      XMLInputFactory inFactory = XMLInputFactory.newInstance();
      inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
      XMLStreamReader parser;
      InputStreamReader in;
      in = new InputStreamReader(new FileInputStream(datei), StandardCharsets.UTF_8);
      parser = inFactory.createXMLStreamReader(in);
      while (parser.hasNext()) {
        event = parser.next();
        if (event == XMLStreamConstants.START_ELEMENT) {
          // String t = parser.getLocalName();
          if (abo && parser.getLocalName().equals(DatenAbo.TAG)) {
            // Abo
            DatenAbo datenAbo = new DatenAbo();
            if (get(parser, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr)) {
              ++found[0];
              daten.getListeAbo().addAbo(datenAbo);
            }
          } else if (black && parser.getLocalName().equals(DatenBlacklist.TAG)) {
            // Blacklist
            ListeBlacklist blacklist = daten.getListeBlacklist();
            DatenBlacklist datenBlacklist = new DatenBlacklist();
            if (get(parser, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, datenBlacklist.arr)) {
              ++found[1];
              blacklist.addWithoutNotification(datenBlacklist);
            }
          } else if (replace && parser.getLocalName().equals(ReplaceList.REPLACELIST)) {
            // Ersetzungstabelle
            String[] sa = new String[ReplaceList.MAX_ELEM];
            if (get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)) {
              ++found[2];
              ReplaceList.list.add(sa);
            }
          }
        }
      }
      in.close();
    } catch (Exception ex) {
      Log.errorLog(302045698, ex);
    }
    if (found[0] > 0) {
      daten.getListeAbo().aenderungMelden();
    }
    if (found[1] > 0) {
      daten.getListeBlacklist().filterListAndNotifyListeners();
    }
    if (found[2] > 0) {
      Listener.notify(Listener.EREIGNIS_REPLACELIST_CHANGED, IoXmlLesen.class.getSimpleName());
    }
    return found;
  }

  // ##############################
  // private
  // ##############################
  private static boolean get(XMLStreamReader parser, String xmlElem, String[] xmlNames,
      String[] strRet) {
    boolean ret = true;
    int maxElem = strRet.length;
    for (int i = 0; i < maxElem; ++i) {
      if (strRet[i] == null) {
        // damit Vorgaben nicht verschwinden!
        strRet[i] = "";
      }
    }
    try {
      while (parser.hasNext()) {
        int event = parser.next();
        if (event == XMLStreamConstants.END_ELEMENT) {
          if (parser.getLocalName().equals(xmlElem)) {
            break;
          }
        }
        if (event == XMLStreamConstants.START_ELEMENT) {
          for (int i = 0; i < maxElem; ++i) {
            if (parser.getLocalName().equals(xmlNames[i])) {
              strRet[i] = parser.getElementText();
              break;
            }
          }
        }
      }
    } catch (Exception ex) {
      ret = false;
      Log.errorLog(739530149, ex);
    }
    return ret;
  }

  private static boolean getConfig(XMLStreamReader parser, String xmlElem) {
    boolean ret = true;
    try {
      while (parser.hasNext()) {
        int event = parser.next();
        if (event == XMLStreamConstants.END_ELEMENT) {
          if (parser.getLocalName().equals(xmlElem)) {
            break;
          }
        }
        if (event == XMLStreamConstants.START_ELEMENT) {
          String s = parser.getLocalName();
          String n = parser.getElementText();
          MVConfig.add(s, n);
        }
      }
    } catch (Exception ex) {
      ret = false;
      Log.errorLog(945120369, ex);
    }
    return ret;
  }

}
