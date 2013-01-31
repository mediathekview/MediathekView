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
package mediathek.daten;

import mediathek.MediathekGui;
import mediathek.controller.io.ErledigteAbos;
import mediathek.controller.io.History;
import mediathek.controller.io.IoXmlLesen;
import mediathek.controller.io.IoXmlSchreiben;
import mediathek.controller.io.starter.StarterClass;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.InfoPanel;
import mediathek.tool.Konstanten;

public final class DDaten extends Daten {
    // Konstanten, Systemeinstellungen und alles was wichtig ist für
    // MediathekGui, MediathekAuto
    // erweitert "Daten" um die Klassen für die Version Gui+Auto
    //
    // Daten.listeFilme sind alle Filme,
    // DDaten.listeFilmeNachBlackList sind die Filme mit denen das Programm arbeitet, eben ohne die Blacklist
    public static ListeFilme listeFilmeNachBlackList = null; 
    public ListeBlacklist listeBlacklist = null;
    public ListePset listePset = null;
    public ListeAbo listeAbo = null;
    public ListeDownloads listeDownloads = null;
    public History history = null;
    public ErledigteAbos erledigteAbos = null;
    // globale Objekte
    public IoXmlLesen ioXmlLesen = null;
    public IoXmlSchreiben ioXmlSchreiben = null;
    public StarterClass starterClass = null; // Klasse zum Ausführen der Programme: VLC, flvstreamer, ...
    public InfoPanel infoPanel = null; // Panel unten in der Gui mit den Infos: Anzahl Filme...
    // Panel
    public MediathekGui mediathekGui = null; // JFrame der Gui
    public GuiFilme guiFilme = null; // Tab mit den Filmen
    public GuiDownloads guiDownloads = null; // Tab mit den Downloads
    public GuiAbo guiAbo = null; // Tab mit den Abos
    // für die Tabellen
    public String[] tabFilmeBreit = new String[DatenFilm.FILME_MAX_ELEM];
    public boolean nachDownloadShutDown = false;

    public DDaten(String basis, boolean gui) {
        super(basis);
        listeFilmeNachBlackList = new ListeFilme();
        listeBlacklist = new ListeBlacklist();
        listePset = new ListePset();
        listeAbo = new ListeAbo(this);
        listeDownloads = new ListeDownloads(this);
        erledigteAbos = new ErledigteAbos(this);
        //initialisieren
        ioXmlLesen = new IoXmlLesen();
        ioXmlSchreiben = new IoXmlSchreiben();
        history = new History(getBasisVerzeichnis(true) + Konstanten.LOG_DATEI_HISTORY);
        starterClass = new StarterClass(this);
        if (gui) {
            infoPanel = new InfoPanel();
        }
    }

    @Override
    public void allesLaden() {
        super.allesLaden();
        ioXmlLesen.datenLesen(this);
        history.laden();
    }

    @Override
    public void allesSpeichern() {
        super.allesSpeichern();
        ioXmlSchreiben.datenSchreiben(this);
    }
}
