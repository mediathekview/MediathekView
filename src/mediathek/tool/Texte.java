/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

public class Texte {

    private static final String ZIELNAME =
            " |  \n"
            + " |  %D Sendedatum des Films oder aktuelles Datum wenn Sendedatum leer\n"
            + " |  %d Sendezeit des Films oder jetzt wenn Sendezeit leer\n"
            + " |  %l Nummer des Films in der Liste aller Filme\n"
            + " |  %s Sender des Films\n"
            + " |  %N Originaldateiname des Films\n"
            + " |  %T Titel des Films\n"
            + " |  %t Thema des Abos\n"
            + " |  %H heute, aktuelles Datum der Form JJJJMMTT zB. 20090815 (15.08.2009)\n"
            + " |  %h jetzt, aktuelle Uhrzeit der Form SSMMss zB. 152059 (15:20:59)\n"
            + " |  \n";
    private static final String PARAMETER_URL =
            " |\n"
            + " |  Diese Angeben werden durch die URL ersetzt:\n"
            + " |  %f ist die Url des Films (Original-URL)\n"
            + " |  %F ist die Url des Films für den flvstreamer vorbereitet\n"
            + " |  %k ist die Url des Films in \"kleiner\" Auflösung, nur 3sat, ZDF, NDR\n"
            + " |\n";
    public static final String PROGRAMMGRUPPE_BUTTON =
            "Die Buttons werden immer in 4 Spalten angezeigt. "
            + "Diese Optionen werden dabei unterstützt:\n"
            + "\n"
            + "Programmgruppe hat keinen Namen:\n"
            + " Platzhalter, es wird nichts angezeigt und das Feld bleibt leer.\n"
            + "\n"
            + "Nur der Name der Programmgruppe ist gefüllt (und keine Programme angelegt):\n"
            + " Label, es wird ein Label mit dem Namen und kein Button angezeigt.\n"
            + "\n"
            + "Alle anderen Programmgruppen werden als Button angezeigt.\n"
            + " Zu jeder Programmgruppe können mehrere Programme angelegt werden.\n"
            + " Diese Programme sind dann diesem Button zugewiesen und das erste\n"
            + " passende wird verwendet.\n"
            + "\n"
            + "Der Parameter \"Schalter\" bezeichnet die Parameter die dem Programm\n"
            + "angehängt werden sollen.\n"
            + "Der Parameter \"Zielpfad\" und \"Zieldateiname\" bezeichnen die Zieldatei.\n"
            + "Der Parameter \"Zieldateiname\" kann sowohl bei einem Programm als auch für eine\n"
            + "Programmguppe angelegt werden. Existiert ein Zieldateiname für das gewählte Programm,\n"
            + "wird dieser verwendet, ansonsten der von der Programmgruppe. So kann für jedes Programm\n"
            + "eine eigene Dateiendung zB. \".mp4\" vergeben werden.\n"
            + "Wird der Button dann gedrückt, wird das hier eingestellte Programm mit\n"
            + "den Parametern gestartet.\n"
            + "\n"
            + "Wiedergeben:\n"
            + "==============\n"
            + " |  Programm: Programmname mit Pfad zB.: /usr/bin/vlc\n"
            + " |  Schalter: Angaben die das Programm zum Wiedergeben braucht.\n"
            //Parameter für die URL des Films
            + PARAMETER_URL
            + " |  Die Felder Zielpfad und Dateiname haben dabei keine Verwendung.\n"
            + " |--------------------------------------------------------------\n"
            + "\n"
            + "Aufzeichnen:\n"
            + "==============\n"
            + " |  Programm: Programmname mit Pfad zB.: /usr/bin/vlc\n"
            + " |  Schalter: Angaben die das Programm zum Aufzeichnen braucht.\n"
            //Parameter für die URL des Films
            + PARAMETER_URL
            + " |  zwei Sterne ** werden durch den Zieldateinamen\n"
            + " |  mit Pfad ersetzt. zB. -playlist %f -dumpstream -dumpfile **\n"
            + " |  Zielpfad: ist der Pfad der Zieldatei.\n"
            + " |  Zieldateiname: ist der Name der Zieldatei. Es sind folgende Angaben möglich:\n"
            //Parameter beim Namen des Films
            + ZIELNAME
            + " |  \n"
            + " |  %n der Name wird in einem Dialog abgefragt\n"
            + " |  %p Pfad und Name der Zieldatei werden in einem Dialog abgefragt,\n"
            + " |  wird %p verwendet, dann wird das Feld Zielpfad als Vorgabe im\n"
            + " |  Auswahldialog eingetragen.\n"
            + " |  \n"
            + " |  Es kann entweder %n oder %p verwendet werden und wird dann\n"
            + " |  vor dem Speichern abgefragt.\n"
            + " |  \n"
            + " |  Wird kein %p verwendet muss ein fester\n"
            + " |  existierender Pfad im Feld Pfad eingetragen werden.\n"
            + " |  \n"
            + " |  \n"
            + " |  Beispiel für den mplayer:\n"
            + " |  Schalter: -playlist %f -dumpstream -dumpfile **\n"
            + " |  Dateiname: %t-%T-%n.mp4\n"
            + " |  hier wier %f durch die URL des Films ersetzt und \n"
            + " |  %t, %T und %n durch einen Pfad und Dateinamen\n"
            + " |  (Thema-Titel-Dateiname_des_Films.mp4).\n"
            + " |--------------------------------------------------------------\n";
    /**
     *
     */
    public static final String PROGRAMMGRUPPE_ABO =
            "Jedes Abo wird einer Programmgruppe zugeordnet. Eine Programmgruppe muss\n"
            + "mindestens ein Programm enthalten. Sind mehrere enthalten, wird beim Download\n"
            + "eines Films das passende Programm ausgewählt.\n"
            + "\n"
            + "Links ist die Liste aller Programmgruppen.\n"
            + "Rechts oben sind die Einstellungen für die Programme und rechts\n"
            + "unten die Einstellungen für die gesamte Programmgruppe.\n"
            + "\n"
            + "Im Schalter für das Programm müssen 2 Sterne ( ** ) enthalten sein.\n"
            + "Hier wird der Pfad + Dateiname eingefügt.\n"
            //Parameter für die URL des Films
            + PARAMETER_URL
            + "Der Parameter \"Zielpfad\" und \"Zieldateiname\" bezeichnen die Zieldatei.\n"
            + "Der Parameter \"Zieldateiname\" kann sowohl bei einem Programm als auch für eine\n"
            + "Programmguppe angelegt werden. Existiert ein Zieldateiname für das gewählte Programm,\n"
            + "wird dieser verwendet, ansonsten der von der Programmgruppe. So kann für jedes Programm\n"
            + "eine eigene Dateiendung zB. \".mp4\" vergeben werden.\n"
            + "Es sind folgende Angaben möglich:\n"
            //Parameter beim Namen des Films
            + ZIELNAME
            + "\n"
            + "Beispiel für den mplayer:\n"
            + "Schalter: -playlist %f -dumpstream -dumpfile **\n"
            + "Dateiname: %t-%T-%n.mp4\n"
            + "hier wier %f durch die URL des Films ersetzt und \n"
            + "%t, %T und %n durch einen Pfad und Dateinamen\n"
            + "(Thema-Titel-Dateiname_des_Films.mp4).\n"
            + "\n"
            + "Der Pfad wird aus dem Zielpfad und den für jedes Abo\n"
            + "angegebenem Pfad gebildet.\n";
}
