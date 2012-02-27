/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.pGruppen;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.CodeSource;
import mediathek.Log;
import mediathek.Main;

/**
 *
 *   @author emil
 */
public class Pgruppen {

    String PFAD_PGRUPPE_LINUX = "/mediathek/pGruppen/pgr_linux.xml";
    String PFAD_PGRUPPE_WINDOWS = "/mediathek/pGruppen/pgr_windows.xml";
    String PFAD_PGRUPPE_MAC = "/mediathek/pGruppen/pgr_mac.xml";

    public InputStream getLinux() {
        try {
            return getClass().getResource(PFAD_PGRUPPE_LINUX).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(this.getClass().getSimpleName(), ex);
        }
        return null;
    }

    public InputStream getWindows() {
        try {
            return getClass().getResource(PFAD_PGRUPPE_WINDOWS).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(this.getClass().getSimpleName(), ex);
        }
        return null;
    }

    public InputStream getMac() {
        try {
            return getClass().getResource(PFAD_PGRUPPE_MAC).openStream();
        } catch (IOException ex) {
            Log.fehlerMeldung(this.getClass().getSimpleName(), ex);
        }
        return null;
    }
}
