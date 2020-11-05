package mediathek.tool;

import mediathek.Main;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.security.CodeSource;

public class MVFunctionSys {
    /**
     * Retrieve the path to the program jar file.
     *
     * @return The program jar file path with a separator added.
     */
    public static String getPathToApplicationJar() {
        // macht Probleme bei Win und Netzwerkpfaden, liefert dann Absolute Pfade zB. \\VBOXSVR\share\Mediathek\...
        String pFilePath = "pFile";
        File propFile = new File(pFilePath);
        if (!propFile.exists()) {
            try {
                CodeSource cS = Main.class.getProtectionDomain().getCodeSource();
                File jarFile = new File(cS.getLocation().toURI().getPath());
                String jarDir = jarFile.getParentFile().getPath();
                propFile = new File(jarDir + File.separator + pFilePath);
            } catch (Exception ignored) {
            }
        }
        String s = StringUtils.replace(propFile.getAbsolutePath(), pFilePath, "");
        if (!s.endsWith(File.separator)) {
            s += File.separator;
        }
        return s;
    }
}
