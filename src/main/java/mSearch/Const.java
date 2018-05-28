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
package mSearch;

import java.util.concurrent.TimeUnit;

public class Const {

    @Deprecated
    public static final String VERSION = "13";
    public static final String PROGRAMMNAME = "MSearch";
    // MediathekView URLs
    public static final String ADRESSE_FILMLISTEN_SERVER_DIFF = "http://res.mediathekview.de/diff.xml";
    public static final String ADRESSE_FILMLISTEN_SERVER_AKT = "http://res.mediathekview.de/akt.xml";
    // Dateien/Verzeichnisse
    public static final String FORMAT_ZIP = ".zip";
    public static final String FORMAT_XZ = ".xz";
    public static final long ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE = TimeUnit.SECONDS.convert(3, TimeUnit.HOURS);
    public static final String TIME_MAX_AGE_FOR_DIFF = "09"; // Uhrzeit ab der die Diffliste alle Änderungen abdeckt, die Filmliste darf also nicht vor xx erstellt worden sein

    public static final String DREISAT = "3Sat";
    public static final String ARD = "ARD";
    public static final String ARTE_DE = "ARTE.DE";
    public static final String ARTE_FR = "ARTE.FR";
    public static final String BR = "BR";
    public static final String DW = "DW";
    public static final String HR = "HR";
    public static final String KIKA = "KiKA";
    public static final String MDR = "MDR";
    public static final String NDR = "NDR";
    public static final String ORF = "ORF";
    public static final String PHOENIX = "PHOENIX";
    public static final String RBB = "RBB";
    public static final String SR = "SR";
    public static final String SRF = "SRF";
    public static final String SRF_PODCAST = "SRF.Podcast";
    public static final String SWR = "SWR";
    public static final String WDR = "WDR";
    public static final String ZDF = "ZDF";
    public static final String ZDF_TIVI = "ZDF-tivi";

    public static final String[] SENDER = {DREISAT, ARD, ARTE_DE, ARTE_FR, BR, DW, HR, KIKA, MDR, NDR, ORF, PHOENIX, RBB, SR, SRF, SRF_PODCAST, SWR, WDR, ZDF, ZDF_TIVI};
}
