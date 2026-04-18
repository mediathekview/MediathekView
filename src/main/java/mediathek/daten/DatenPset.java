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
 // *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.MVMessageDialog;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import java.awt.*;
import java.util.Optional;

public class DatenPset implements Comparable<DatenPset> {

    //Tags Programmgruppen
    public static final int PROGRAMMSET_NAME = 0;
    public static final int PROGRAMMSET_PRAEFIX_DIREKT = 1;
    public static final int PROGRAMMSET_SUFFIX_DIREKT = 2;
    public static final int PROGRAMMSET_FARBE = 3;
    public static final int PROGRAMMSET_ZIEL_PFAD = 4;
    public static final int PROGRAMMSET_ZIEL_DATEINAME = 5;
    public static final int PROGRAMMSET_THEMA_ANLEGEN = 6;
    public static final int PROGRAMMSET_IST_ABSPIELEN = 7;
    public static final int PROGRAMMSET_IST_SPEICHERN = 8;
    public static final int PROGRAMMSET_IST_BUTTON = 9;
    public static final int PROGRAMMSET_IST_ABO = 10;
    public static final int PROGRAMMSET_LAENGE_BESCHRAENKEN = 11;
    public static final int PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN = 12;
    public static final int PROGRAMMSET_MAX_LAENGE = 13;
    public static final int PROGRAMMSET_MAX_LAENGE_FIELD = 14;
    public static final int PROGRAMMSET_AUFLOESUNG = 15;
    public static final int PROGRAMMSET_ADD_ON = 16;
    public static final int PROGRAMMSET_BESCHREIBUNG = 17;
    public static final int PROGRAMMSET_INFODATEI = 19;
    public static final int PROGRAMMSET_SPOTLIGHT = 20;
    public static final int PROGRAMMSET_SUBTITLE = 21;

    public static final String TAG = "Programmset";
    public static final int MAX_ELEM = 22;

    public static final String[] COLUMN_NAMES = {"Setname", "Präfix", "Suffix", "Farbe", "Zielpfad", "Zieldateiname", "Thema anlegen",
            "Abspielen", "Speichern", "Button", "Abo", "Länge", "Länge Feld", "max Länge", "max Länge Feld", "Auflösung", "AddOn",
            "Beschreibung", "Url Info", "Infodatei", "Spotlight", "Untertitel"};
    public static final String[] XML_NAMES = {"Name", "Praefix", "Suffix", "Farbe", "Zielpfad", "Zieldateiname", "Thema-anlegen",
            "Abspielen", "Speichern", "Button", "Abo", "Laenge", "Laenge-Feld", "max-Laenge", "max-Laenge-Feld", "Aufloesung", "AddOn",
            "Beschreibung", "Info-URL", "Infodatei", "Spotlight", "Untertitel"};
    private static final Logger logger = LogManager.getLogger();
    private final ListeProg listeProg = new ListeProg();
    private String name;
    private String praefixDirekt;
    private String suffixDirekt;
    private Color farbe;
    private String zielPfad;
    private String zielDateiname;
    private Boolean themaAnlegen;
    private Boolean istAbspielen;
    private Boolean istSpeichern;
    private Boolean istButton;
    private Boolean istAbo;
    private Boolean laengeBeschraenken;
    private Boolean laengeFieldBeschraenken;
    private Integer maxLaenge;
    private Integer maxLaengeField;
    private FilmResolution.Enum aufloesung;
    private String addOn;
    private String beschreibung;
    private String infoUrl;
    private Boolean infodatei;
    private Boolean spotlight;
    private Boolean subtitle;

    public DatenPset() {
        initialize();
    }

    public DatenPset(String name) {
        // neue Pset sind immer gleich Button
        initialize();
        set(PROGRAMMSET_NAME, name);
        set(PROGRAMMSET_IST_BUTTON, Boolean.TRUE.toString());
    }

    /**
     * Indicate whether a subtitle file should be downloaded.
     * @return true if download, otherwise false.
     */
    public boolean shouldDownloadSubtitle() {
        return Boolean.TRUE.equals(subtitle);
    }

    /**
     * Indicate wheter an Infofile should be created.
     * @return true for creation, false otherwise
     */
    public boolean shouldCreateInfofile() {
        return Boolean.TRUE.equals(infodatei);
    }

    public String getPraefixDirekt() {
        return praefixDirekt;
    }

    public String getSuffixDirekt() {
        return suffixDirekt;
    }

    public String getBeschreibung() {
        return beschreibung;
    }

    public void setBeschreibung(String beschreibung) {
        this.beschreibung = beschreibung == null ? "" : beschreibung;
    }

    public boolean isThemaAnlegen() {
        return Boolean.TRUE.equals(themaAnlegen);
    }

    public void setThemaAnlegen(boolean themaAnlegen) {
        this.themaAnlegen = themaAnlegen;
    }

    public boolean isLaengeBeschraenken() {
        return Boolean.TRUE.equals(laengeBeschraenken);
    }

    public void setLaengeBeschraenken(boolean laengeBeschraenken) {
        this.laengeBeschraenken = laengeBeschraenken;
    }

    public boolean isLaengeFieldBeschraenken() {
        return Boolean.TRUE.equals(laengeFieldBeschraenken);
    }

    public void setLaengeFieldBeschraenken(boolean laengeFieldBeschraenken) {
        this.laengeFieldBeschraenken = laengeFieldBeschraenken;
    }

    public Integer getMaxLaenge() {
        return maxLaenge;
    }

    public void setMaxLaenge(Integer maxLaenge) {
        this.maxLaenge = maxLaenge;
    }

    public Integer getMaxLaengeField() {
        return maxLaengeField;
    }

    public void setMaxLaengeField(Integer maxLaengeField) {
        this.maxLaengeField = maxLaengeField;
    }

    public FilmResolution.Enum getAufloesung() {
        return aufloesung;
    }

    public void setAufloesung(FilmResolution.Enum aufloesung) {
        this.aufloesung = aufloesung == null ? FilmResolution.Enum.NORMAL : aufloesung;
    }

    public boolean isSpotlight() {
        return Boolean.TRUE.equals(spotlight);
    }

    public void setSpotlight(boolean spotlight) {
        this.spotlight = spotlight;
    }

    public void setInfodatei(boolean infodatei) {
        this.infodatei = infodatei;
    }

    public void setSubtitle(boolean subtitle) {
        this.subtitle = subtitle;
    }

    public String getAddOn() {
        return addOn;
    }

    public void setName(String name) {
        this.name = name == null ? "" : name;
    }

    public void clearFarbe() {
        this.farbe = null;
    }

    public String get(int index) {
        return switch (index) {
            case PROGRAMMSET_NAME -> name;
            case PROGRAMMSET_PRAEFIX_DIREKT -> praefixDirekt;
            case PROGRAMMSET_SUFFIX_DIREKT -> suffixDirekt;
            case PROGRAMMSET_FARBE -> colorToString(farbe);
            case PROGRAMMSET_ZIEL_PFAD -> zielPfad;
            case PROGRAMMSET_ZIEL_DATEINAME -> zielDateiname;
            case PROGRAMMSET_THEMA_ANLEGEN -> boolToString(themaAnlegen);
            case PROGRAMMSET_IST_ABSPIELEN -> boolToString(istAbspielen);
            case PROGRAMMSET_IST_SPEICHERN -> boolToString(istSpeichern);
            case PROGRAMMSET_IST_BUTTON -> boolToString(istButton);
            case PROGRAMMSET_IST_ABO -> boolToString(istAbo);
            case PROGRAMMSET_LAENGE_BESCHRAENKEN -> boolToString(laengeBeschraenken);
            case PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN -> boolToString(laengeFieldBeschraenken);
            case PROGRAMMSET_MAX_LAENGE -> intToString(maxLaenge);
            case PROGRAMMSET_MAX_LAENGE_FIELD -> intToString(maxLaengeField);
            case PROGRAMMSET_AUFLOESUNG -> aufloesung == null ? "" : aufloesung.toString();
            case PROGRAMMSET_ADD_ON -> addOn;
            case PROGRAMMSET_BESCHREIBUNG -> beschreibung;
            case 18 -> infoUrl;
            case PROGRAMMSET_INFODATEI -> boolToString(infodatei);
            case PROGRAMMSET_SPOTLIGHT -> boolToString(spotlight);
            case PROGRAMMSET_SUBTITLE -> boolToString(subtitle);
            default -> throw new ArrayIndexOutOfBoundsException(index);
        };
    }

    public void set(int index, String value) {
        final String normalizedValue = value == null ? "" : value;

        switch (index) {
            case PROGRAMMSET_NAME -> name = normalizedValue;
            case PROGRAMMSET_PRAEFIX_DIREKT -> praefixDirekt = normalizedValue;
            case PROGRAMMSET_SUFFIX_DIREKT -> suffixDirekt = normalizedValue;
            case PROGRAMMSET_FARBE -> farbe = parseColor(normalizedValue);
            case PROGRAMMSET_ZIEL_PFAD -> zielPfad = normalizedValue;
            case PROGRAMMSET_ZIEL_DATEINAME -> zielDateiname = normalizedValue;
            case PROGRAMMSET_THEMA_ANLEGEN -> themaAnlegen = parseBoolean(normalizedValue);
            case PROGRAMMSET_IST_ABSPIELEN -> istAbspielen = parseBoolean(normalizedValue);
            case PROGRAMMSET_IST_SPEICHERN -> istSpeichern = parseBoolean(normalizedValue);
            case PROGRAMMSET_IST_BUTTON -> istButton = parseBoolean(normalizedValue);
            case PROGRAMMSET_IST_ABO -> istAbo = parseBoolean(normalizedValue);
            case PROGRAMMSET_LAENGE_BESCHRAENKEN -> laengeBeschraenken = parseBoolean(normalizedValue);
            case PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN -> laengeFieldBeschraenken = parseBoolean(normalizedValue);
            case PROGRAMMSET_MAX_LAENGE -> maxLaenge = parseInteger(normalizedValue);
            case PROGRAMMSET_MAX_LAENGE_FIELD -> maxLaengeField = parseInteger(normalizedValue);
            case PROGRAMMSET_AUFLOESUNG -> aufloesung = normalizedValue.isEmpty() ? null : FilmResolution.Enum.fromLegacyString(normalizedValue);
            case PROGRAMMSET_ADD_ON -> addOn = normalizedValue;
            case PROGRAMMSET_BESCHREIBUNG -> beschreibung = normalizedValue;
            case 18 -> infoUrl = normalizedValue;
            case PROGRAMMSET_INFODATEI -> infodatei = parseBoolean(normalizedValue);
            case PROGRAMMSET_SPOTLIGHT -> spotlight = parseBoolean(normalizedValue);
            case PROGRAMMSET_SUBTITLE -> subtitle = parseBoolean(normalizedValue);
            default -> throw new ArrayIndexOutOfBoundsException(index);
        }
    }

    public String[] toArray() {
        String[] values = new String[MAX_ELEM];
        values[PROGRAMMSET_NAME] = getName();
        values[PROGRAMMSET_PRAEFIX_DIREKT] = getPraefixDirekt();
        values[PROGRAMMSET_SUFFIX_DIREKT] = getSuffixDirekt();
        values[PROGRAMMSET_FARBE] = colorToString(farbe);
        values[PROGRAMMSET_ZIEL_PFAD] = getZielPfad();
        values[PROGRAMMSET_ZIEL_DATEINAME] = getZielDateiname();
        values[PROGRAMMSET_THEMA_ANLEGEN] = boolToString(themaAnlegen);
        values[PROGRAMMSET_IST_ABSPIELEN] = boolToString(istAbspielen);
        values[PROGRAMMSET_IST_SPEICHERN] = boolToString(istSpeichern);
        values[PROGRAMMSET_IST_BUTTON] = boolToString(istButton);
        values[PROGRAMMSET_IST_ABO] = boolToString(istAbo);
        values[PROGRAMMSET_LAENGE_BESCHRAENKEN] = boolToString(laengeBeschraenken);
        values[PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN] = boolToString(laengeFieldBeschraenken);
        values[PROGRAMMSET_MAX_LAENGE] = intToString(maxLaenge);
        values[PROGRAMMSET_MAX_LAENGE_FIELD] = intToString(maxLaengeField);
        values[PROGRAMMSET_AUFLOESUNG] = aufloesung == null ? "" : aufloesung.toString();
        values[PROGRAMMSET_ADD_ON] = getAddOn();
        values[PROGRAMMSET_BESCHREIBUNG] = getBeschreibung();
        values[18] = infoUrl;
        values[PROGRAMMSET_INFODATEI] = boolToString(infodatei);
        values[PROGRAMMSET_SPOTLIGHT] = boolToString(spotlight);
        values[PROGRAMMSET_SUBTITLE] = boolToString(subtitle);
        return values;
    }

    public void copyFrom(String[] values) {
        clearFields();
        if (values == null) {
            initializeDefaults();
            return;
        }

        if (values.length > PROGRAMMSET_NAME) {
            setName(values[PROGRAMMSET_NAME]);
        }
        if (values.length > PROGRAMMSET_PRAEFIX_DIREKT) {
            set(PROGRAMMSET_PRAEFIX_DIREKT, values[PROGRAMMSET_PRAEFIX_DIREKT]);
        }
        if (values.length > PROGRAMMSET_SUFFIX_DIREKT) {
            set(PROGRAMMSET_SUFFIX_DIREKT, values[PROGRAMMSET_SUFFIX_DIREKT]);
        }
        if (values.length > PROGRAMMSET_FARBE) {
            set(PROGRAMMSET_FARBE, values[PROGRAMMSET_FARBE]);
        }
        if (values.length > PROGRAMMSET_ZIEL_PFAD) {
            setZielPfad(values[PROGRAMMSET_ZIEL_PFAD]);
        }
        if (values.length > PROGRAMMSET_ZIEL_DATEINAME) {
            setZielDateiname(values[PROGRAMMSET_ZIEL_DATEINAME]);
        }
        if (values.length > PROGRAMMSET_THEMA_ANLEGEN) {
            set(PROGRAMMSET_THEMA_ANLEGEN, values[PROGRAMMSET_THEMA_ANLEGEN]);
        }
        if (values.length > PROGRAMMSET_IST_ABSPIELEN) {
            set(PROGRAMMSET_IST_ABSPIELEN, values[PROGRAMMSET_IST_ABSPIELEN]);
        }
        if (values.length > PROGRAMMSET_IST_SPEICHERN) {
            setSpeichern(Boolean.parseBoolean(values[PROGRAMMSET_IST_SPEICHERN]));
        }
        if (values.length > PROGRAMMSET_IST_BUTTON) {
            setButton(Boolean.parseBoolean(values[PROGRAMMSET_IST_BUTTON]));
        }
        if (values.length > PROGRAMMSET_IST_ABO) {
            setAbo(Boolean.parseBoolean(values[PROGRAMMSET_IST_ABO]));
        }
        if (values.length > PROGRAMMSET_LAENGE_BESCHRAENKEN) {
            setLaengeBeschraenken(Boolean.parseBoolean(values[PROGRAMMSET_LAENGE_BESCHRAENKEN]));
        }
        if (values.length > PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN) {
            setLaengeFieldBeschraenken(Boolean.parseBoolean(values[PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN]));
        }
        if (values.length > PROGRAMMSET_MAX_LAENGE) {
            set(PROGRAMMSET_MAX_LAENGE, values[PROGRAMMSET_MAX_LAENGE]);
        }
        if (values.length > PROGRAMMSET_MAX_LAENGE_FIELD) {
            set(PROGRAMMSET_MAX_LAENGE_FIELD, values[PROGRAMMSET_MAX_LAENGE_FIELD]);
        }
        if (values.length > PROGRAMMSET_AUFLOESUNG) {
            set(PROGRAMMSET_AUFLOESUNG, values[PROGRAMMSET_AUFLOESUNG]);
        }
        if (values.length > PROGRAMMSET_ADD_ON) {
            set(PROGRAMMSET_ADD_ON, values[PROGRAMMSET_ADD_ON]);
        }
        if (values.length > PROGRAMMSET_BESCHREIBUNG) {
            setBeschreibung(values[PROGRAMMSET_BESCHREIBUNG]);
        }
        if (values.length > 18) {
            set(18, values[18]);
        }
        if (values.length > PROGRAMMSET_INFODATEI) {
            setInfodatei(Boolean.parseBoolean(values[PROGRAMMSET_INFODATEI]));
        }
        if (values.length > PROGRAMMSET_SPOTLIGHT) {
            setSpotlight(Boolean.parseBoolean(values[PROGRAMMSET_SPOTLIGHT]));
        }
        if (values.length > PROGRAMMSET_SUBTITLE) {
            setSubtitle(Boolean.parseBoolean(values[PROGRAMMSET_SUBTITLE]));
        }
        initializeDefaults();
    }

    public void addProg(DatenProg prog) {
        listeProg.add(prog);
    }

    public ListeProg getListeProg() {
        return listeProg;
    }

    public DatenProg getProg(int i) {
        return listeProg.get(i);
    }

    public boolean progsContainPath() {
        // ein Programmschalter mit
        // "**" (Pfad/Datei) oder %a (Pfad) oder %b (Datei)
        // damit ist es ein Set zum Speichern
        boolean ret = false;
        for (DatenProg prog : listeProg) {
            if (prog.arr[DatenProg.PROGRAMM_SCHALTER].contains("**")
                    || prog.arr[DatenProg.PROGRAMM_SCHALTER].contains("%a")
                    || prog.arr[DatenProg.PROGRAMM_SCHALTER].contains("%b")) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    public boolean isEmpty() {
        boolean ret = true;
        for (String s : toArray()) {
            if (!s.isEmpty()) {
                ret = false;
            }
        }
        if (!listeProg.isEmpty()) {
            ret = false;
        }
        return ret;
    }

    public boolean istAbspielen() {
        return Boolean.TRUE.equals(istAbspielen);
    }

    public void setAbspielen(boolean value) {
        istAbspielen = value;
    }

    public boolean istSpeichern() {
        return Boolean.TRUE.equals(istSpeichern);
    }

    public boolean istButton() {
        return Boolean.TRUE.equals(istButton);
    }

    public boolean istAbo() {
        return Boolean.TRUE.equals(istAbo);
    }

    /**
     * Is this pSet a label?
     * @return true if it is a label, otherwise false
     */
    public boolean isLabel() {
        //if program list is empty AND there is a name -> label
        if (this.listeProg.isEmpty()) {
            return !getName().isEmpty();
        }
        return false;
    }

    public boolean isFreeLine() {
        //Wenn die Programmgruppe keinen Namen hat, leere Zeile
        return getName().isEmpty();
    }

    public void setSpeichern(boolean set) {
        set(DatenPset.PROGRAMMSET_IST_SPEICHERN, Boolean.toString(set));
    }

    public void setButton(boolean set) {
        set(DatenPset.PROGRAMMSET_IST_BUTTON, Boolean.toString(set));
    }

    public void setAbo(boolean set) {
        set(DatenPset.PROGRAMMSET_IST_ABO, Boolean.toString(set));
    }

    public DatenProg getProgUrl(String url) {
        //mit einer Url das Passende Programm finden
        //passt nichts, wird das letzte Programm genommen
        //ist nur ein Programm in der Liste wird dieses genommen
        DatenProg ret = null;
        if (listeProg.isEmpty()) {
            MVMessageDialog.showMessageDialog(null, "Programme einrichten!",
                    "Kein Programm", JOptionPane.INFORMATION_MESSAGE);
        } else if (listeProg.size() == 1) {
            ret = listeProg.getFirst();
        } else {
            for (DatenProg prog : listeProg) {
                if (prog.urlTesten(url)) {
                    ret = prog;
                    break;
                }
            }
            if (!listeProg.isEmpty() && ret == null) {
                ret = listeProg.getLast();
            }
        }
        return ret;
    }

    public String getZielDateiname(String url) {
        //gibt den Zieldateinamen für den Film zurück
        DatenProg prog = this.getProgUrl(url);
        String ret = get(PROGRAMMSET_ZIEL_DATEINAME);
        if (!checkDownloadDirekt(url) && prog != null) {
            // nur wenn kein direkter Download und ein passendes Programm
            if (!prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME].equals("")) {
                ret = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME];
            }
        }
        return ret;
    }

    public String getZielDateiname() {
        return zielDateiname;
    }

    public void setZielDateiname(String zielDateiname) {
        this.zielDateiname = zielDateiname == null ? "" : zielDateiname;
    }

    public String getZielPfad() {
        //gibt den Zielpfad für den Film zurück
        return zielPfad;
    }

    public void setZielPfad(String zielPfad) {
        this.zielPfad = zielPfad == null ? "" : zielPfad;
    }

    public DatenPset copy() {
        DatenPset ret = new DatenPset();
        ret.copyFrom(this.toArray());
        //es darf nur einen geben!
        ret.setName("Kopie-" + getName());
        ret.set(PROGRAMMSET_IST_ABSPIELEN, Boolean.toString(false));
        for (DatenProg prog : getListeProg()) {
            ret.addProg(prog.copy());
        }
        return ret;
    }

    /**
     * Return the specified foreground color if present.
     * @return the requested foreground color for the PSet.
     */
    public Optional<Color> getForegroundColor() {
        return Optional.ofNullable(getFarbe());
    }

    public String getName() {
        return get(PROGRAMMSET_NAME);
    }

    public Color getFarbe() {
        return farbe;
    }

    public void setFarbe(Color farbe) {
        this.farbe = farbe;
    }

    public boolean checkDownloadDirekt(String url) {
        //auf direkte prüfen, pref oder suf: wenn angegeben dann muss es stimmen
        if (!this.getPraefixDirekt().equals("")
                || !this.getSuffixDirekt().equals("")) {
            return GuiFunktionenProgramme.checkPrefix(this.getPraefixDirekt(), url)
                    && GuiFunktionenProgramme.checkSuffix(this.getSuffixDirekt(), url);
        }

        return false;
    }

    @Override
    public String toString() {
        String ret = "";
        ret += "================================================" + System.lineSeparator();
        ret += "| Programmset" + System.lineSeparator();
        for (int i = 0; i < MAX_ELEM; ++i) {
            ret += "| " + COLUMN_NAMES[i] + ": " + get(i) + System.lineSeparator();
        }
        for (Object aListeProg : listeProg) {
            ret += "|" + System.lineSeparator();
            ret += aListeProg.toString();
        }
        ret += "|_______________________________________________" + System.lineSeparator();
        return ret;
    }

    private void initialize() {
        clearFields();
        initializeDefaults();
    }

    private void clearFields() {
        name = "";
        praefixDirekt = "";
        suffixDirekt = "";
        farbe = null;
        zielPfad = "";
        zielDateiname = "";
        themaAnlegen = null;
        istAbspielen = null;
        istSpeichern = null;
        istButton = null;
        istAbo = null;
        laengeBeschraenken = null;
        laengeFieldBeschraenken = null;
        maxLaenge = null;
        maxLaengeField = null;
        aufloesung = null;
        addOn = "";
        beschreibung = "";
        infoUrl = "";
        infodatei = null;
        spotlight = null;
        subtitle = null;
    }

    private void initializeDefaults() {
        if (get(PROGRAMMSET_THEMA_ANLEGEN).isEmpty()) {
            themaAnlegen = Boolean.TRUE;
        }
        if (get(PROGRAMMSET_IST_ABSPIELEN).isEmpty()) {
            istAbspielen = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_IST_SPEICHERN).isEmpty()) {
            istSpeichern = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_IST_BUTTON).isEmpty()) {
            istButton = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_IST_ABO).isEmpty()) {
            istAbo = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_LAENGE_BESCHRAENKEN).isEmpty()) {
            laengeBeschraenken = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_LAENGE_FIELD_BESCHRAENKEN).isEmpty()) {
            laengeFieldBeschraenken = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_INFODATEI).isEmpty()) {
            infodatei = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_SPOTLIGHT).isEmpty()) {
            spotlight = SystemUtils.IS_OS_MAC_OSX;
        }
        if (get(PROGRAMMSET_SUBTITLE).isEmpty()) {
            subtitle = Boolean.FALSE;
        }
        if (get(PROGRAMMSET_AUFLOESUNG).isEmpty()) {
            aufloesung = FilmResolution.Enum.NORMAL;
        }
    }

    private static String boolToString(Boolean value) {
        return value == null ? "" : Boolean.toString(value);
    }

    private static Boolean parseBoolean(String value) {
        return value.isEmpty() ? null : Boolean.parseBoolean(value);
    }

    private static String intToString(Integer value) {
        return value == null ? "" : Integer.toString(value);
    }

    private static Integer parseInteger(String value) {
        return value.isEmpty() ? null : Integer.parseInt(value);
    }

    private static String colorToString(Color value) {
        return value == null ? "" : value.getRed() + "," + value.getGreen() + "," + value.getBlue();
    }

    private Color parseColor(String value) {
        if (value.isEmpty()) {
            return null;
        }
        try {
            final var rgb = value.split(",", 3);
            if (rgb.length != 3) {
                throw new IllegalArgumentException("Invalid RGB color: " + value);
            }
            return new Color(Integer.parseInt(rgb[0]), Integer.parseInt(rgb[1]), Integer.parseInt(rgb[2]));
        } catch (Exception ex) {
            logger.error("getFarbe()", ex);
            return null;
        }
    }

    @Override
    public int compareTo(@NonNull DatenPset o) {
        return 0;
    }
}
