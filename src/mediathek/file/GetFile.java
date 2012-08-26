/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.file;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

/**
 *
 * @author emil
 */
public class GetFile {

    String PFAD_PSET_LINUX = "/mediathek/file/pset_linux.xml";
    String PFAD_PSET_WINDOWS = "/mediathek/file/pset_windows.xml";
    String PFAD_PSET_MAC = "/mediathek/file/pset_mac.xml";
    public static String PFAD_HILFETEXT_SUCHEN = "/mediathek/file/hilfetext_suchen.txt";
    public static String PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_programme.txt";
    public static String PFAD_HILFETEXT_STANDARD_PSET = "hilfetext_standardPset.txt";

    public String getHilfeSuchen(String pfad) {
        String ret = "";
        try {
            InputStreamReader in = new InputStreamReader(getClass().getResource(pfad).openStream(), Konstanten.KODIERUNG_UTF);
            BufferedReader br = new BufferedReader(in);
            String strLine;
            while ((strLine = br.readLine()) != null) {
                ret = ret + "\n" + strLine;
            }
            //Close the input stream
            in.close();
        } catch (IOException ex) {
            Log.fehlerMeldung(885692213,this.getClass().getSimpleName() + ".getHilfeSuchen", ex);
        }
        return ret;
    }

    public InputStream getPsetVorlageLinux() {
        try {
            return getClass().getResource(PFAD_PSET_LINUX).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(469691002,this.getClass().getSimpleName() + ".getLinux", ex);
        }
        return null;
    }

    public InputStream getPsetVorlageWindows() {
        try {
            return getClass().getResource(PFAD_PSET_WINDOWS).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(842306087,this.getClass().getSimpleName() + ".getWindows", ex);
        }
        return null;
    }

    public InputStream getPsetVorlageMac() {
        try {
            return getClass().getResource(PFAD_PSET_MAC).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(496532180,this.getClass().getSimpleName() + ".getMac", ex);
        }
        return null;
    }
}
