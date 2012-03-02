/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.file;

import java.io.*;
import mediathek.Konstanten;
import mediathek.Log;

/**
 *
 * @author emil
 */
public class GetFile {

    String PFAD_PGRUPPE_LINUX = "/mediathek/file/pgr_linux.xml";
    String PFAD_PGRUPPE_WINDOWS = "/mediathek/file/pgr_windows.xml";
    String PFAD_PGRUPPE_MAC = "/mediathek/file/pgr_mac.xml";
    public static String PFAD_HILFETEXT_SUCHEN = "/mediathek/file/hilfetext_suchen.txt";
    public static String PFAD_HILFETEXT_PRGRAMME = "/mediathek/file/hilfetext_programme.txt";
    public static String PFAD_HILFETEXT_STANDARD_PGRUPPEN = "hilfetext_standardPgruppen.txt";

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
            Log.fehlerMeldung(this.getClass().getSimpleName() + ".getHilfeSuchen", ex);
        }
        return ret;
    }

    public InputStream getLinux() {
        try {
            return getClass().getResource(PFAD_PGRUPPE_LINUX).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(this.getClass().getSimpleName() + ".getLinux", ex);
        }
        return null;
    }

    public InputStream getWindows() {
        try {
            return getClass().getResource(PFAD_PGRUPPE_WINDOWS).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(this.getClass().getSimpleName() + ".getWindows", ex);
        }
        return null;
    }

    public InputStream getMac() {
        try {
            return getClass().getResource(PFAD_PGRUPPE_MAC).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(this.getClass().getSimpleName() + ".getMac", ex);
        }
        return null;
    }
}
