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
    String PFAD_HILFETEXT_SUCHEN = "/mediathek/file/hilfetext_suchen.txt";

    public String getHilfeSuchen() {
        String ret = "";
        try {
            InputStreamReader in = new InputStreamReader(getClass().getResource(PFAD_HILFETEXT_SUCHEN).openStream(), Konstanten.KODIERUNG_UTF);
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
