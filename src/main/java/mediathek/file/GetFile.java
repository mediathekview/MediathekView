package mediathek.file;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

/**
 * @author emil
 */
public class GetFile {

    public static final String PFAD_HILFETEXT_GEO = "/mediathek/file/hilfetext_geo.txt";
    public static final String PFAD_HILFETEXT_BLACKLIST = "/mediathek/file/hilfetext_blacklist.txt";
    public static final String PFAD_HILFETEXT_BEENDEN = "/mediathek/file/hilfetext_beenden.txt";
    public static final String PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_pset.txt";
    public static final String PFAD_HILFETEXT_STANDARD_PSET = "hilfetext_standardPset.txt";
    public static final String PFAD_HILFETEXT_EDIT_DOWNLOAD_PROG = "hilfetext_editDownloadProg.txt";
    public static final String PFAD_HILFETEXT_RESET = "hilfetext_reset.txt";
    public static final String PFAD_HILFETEXT_RESET_SET = "hilfetext_reset_set.txt";
    public static final String PFAD_HILFETEXT_DIALOG_MEDIA_DB = "hilfetext_dialog_mediaDb.txt";
    public static final String PFAD_HILFETEXT_PANEL_MEDIA_DB = "hilfetext_panel_mediaDb.txt";
    public static final String PFAD_HILFETEXT_DIALOG_ADD_ABO = "hilfetext_dialog_add_abo.txt";
    private static final String PFAD_PSET_LINUX = "/mediathek/file/pset_linux.xml";
    private static final String PFAD_PSET_WINDOWS = "/mediathek/file/pset_windows.xml";
    private static final String PFAD_PSET_MAC = "/mediathek/file/pset_mac.xml";
    private static final Logger logger = LogManager.getLogger();

    public String getHilfeSuchen(String pfad) {
        String ret = "";
        try (InputStreamReader in = new InputStreamReader(getClass().getResource(pfad).openStream(), StandardCharsets.UTF_8);
             BufferedReader br = new BufferedReader(in)) {
            String strLine;
            while ((strLine = br.readLine()) != null) {
                ret = ret + '\n' + strLine;
            }
        } catch (IOException ex) {
            logger.error("getHilfeSuchen()", ex);
        }
        return ret;
    }

    public InputStreamReader getPsetVorlageLinux() {
        try {
            return new InputStreamReader(getClass().getResource(PFAD_PSET_LINUX).openStream(), StandardCharsets.UTF_8);
        } catch (IOException ex) {
            logger.error("getPsetVorlageLinux()",ex);
        }
        return null;
    }

    public InputStreamReader getPsetVorlageWindows() {
        try {
            return new InputStreamReader(getClass().getResource(PFAD_PSET_WINDOWS).openStream(), StandardCharsets.UTF_8);
        } catch (IOException ex) {
            logger.error("getPsetVorlageWindows()", ex);
        }
        return null;
    }

    public InputStreamReader getPsetVorlageMac() {
        try {
            return new InputStreamReader(getClass().getResource(PFAD_PSET_MAC).openStream(), StandardCharsets.UTF_8);
        } catch (IOException ex) {
            logger.error("getPsetVorlageMac()",ex);
        }
        return null;
    }
}
