package mediathek.tool;

import com.jidesoft.utils.SystemInfo;
import mediathek.daten.Daten;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;

/**
 * User: crystalpalace1977
 * Date: 28.12.14
 * Time: 16:02
 */
public class FilenameUtils {

    /**
     * Valid characters for Windows in file names:
     * Based on http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
     */
    private static final String REGEXP_ILLEGAL_CHARACTERS_WINDOWS = "[:\\\\/*?|<>\"]";
    /**
     * Valid characters for all UNIX-like OS.
     */
    private static final String REGEXP_ILLEGAL_CHARACTERS_OTHERS = "[:\\\\/*|<>]";

    public static String checkDateiname(final String name, final boolean isPath) {
        boolean isWindowsPath = false;
        String ret = name;

        if (SystemInfo.isWindows()) {
            ret = removeWindowsTrailingDots(ret);
        }

        if (isPath) {
            if (File.separator.equals("/")) {
                ret = ret.replace("\\", "-");
            } else {
                ret = ret.replace("/", "-");
            }

            if (SystemInfo.isWindows()) {
                if (ret.length() > 1 && ret.charAt(1) == ':') {
                    // damit auch "d:" und nicht nur "d:\" als Pfad geht
                    isWindowsPath = true;
                    ret = ret.replaceFirst(":", ""); // muss zum Schluss wieder rein, kann aber so nicht ersetzt werden
                }
            }
        } else {
            ret = ret.replaceAll("[\\/]", "-");
        }

        ret = convertToNativeEncoding(ret);

        if (isWindowsPath) {
            // c: wieder herstellen
            if (ret.length() == 1) {
                ret = ret + ":";
            } else if (ret.length() > 1) {
                ret = ret.charAt(0) + ":" + ret.substring(1);
            }
        }

        return ret;
    }

    /**
     * Remove stray trailing dots from string when we are on Windows OS.
     *
     * @param fileName A filename string that might include trailing dots.
     * @return Cleanup string with no dots anymore.
     */
    private static String removeWindowsTrailingDots(String fileName) {
        while (!fileName.isEmpty() && fileName.endsWith(".")) {
            fileName = fileName.substring(0, fileName.length() - 1);
        }
        return fileName;
    }

    /**
     * Convert a filename from Java´s native UTF-16 to OS native character encoding.
     *
     * @param fileName The UTF-16 filename string.
     * @return Natively encoded string for the OS.
     */
    private static String convertToNativeEncoding(String fileName) {
        String ret = fileName;

        ret = removeIllegalCharacters(ret);

        //convert our filename to OS encoding...
        try {
            final CharsetEncoder charsetEncoder = Charset.defaultCharset().newEncoder();
            final ByteBuffer buf = charsetEncoder.encode(CharBuffer.wrap(ret));
            if (buf.hasArray()) {
                ret = new String(buf.array());
            }

            //remove NUL character from conversion...
            ret = ret.replaceAll("\\u0000", "");
        } catch (CharacterCodingException e) {
            e.printStackTrace();
        }

        return ret;
    }

    /**
     * Remove illegal characters from String based on current OS.
     *
     * @param input The input string
     * @return Cleaned-up string.
     */
    private static String removeIllegalCharacters(final String input) {
        String ret = input;

        if (SystemInfo.isWindows()) {
            ret = removeWindowsTrailingDots(ret);
            ret = ret.replaceAll(REGEXP_ILLEGAL_CHARACTERS_WINDOWS, "_");
        } else {
            //not on windows should be a UNIX equivalent, so we are safe :)
            //we can handle double quotes and whitespaces...
            ret = ret.replaceAll(REGEXP_ILLEGAL_CHARACTERS_OTHERS, "_");
        }

        return ret;
    }

    /**
     * Convert a filename from Java´s native UTF-16 to US-ASCII character encoding.
     *
     * @param fileName The UTF-16 filename string.
     * @return US-ASCII encoded string for the OS.
     */
    private static String convertToASCIIEncoding(String fileName) {
        String ret = fileName;

        ret = removeIllegalCharacters(ret);

        ret = ret.replace("ä", "ae");
        ret = ret.replace("ö", "oe");
        ret = ret.replace("ü", "ue");
        ret = ret.replace("Ä", "Ae");
        ret = ret.replace("Ö", "Oe");
        ret = ret.replace("Ü", "Ue");

        //convert our filename to OS encoding...
        try {
            final CharsetEncoder charsetEncoder = Charset.forName("US-ASCII").newEncoder();
            charsetEncoder.onMalformedInput(CodingErrorAction.REPLACE); // otherwise breaks on first unconvertable char
            charsetEncoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
            charsetEncoder.replaceWith(new byte[]{'_'});

            final ByteBuffer buf = charsetEncoder.encode(CharBuffer.wrap(ret));
            if (buf.hasArray()) {
                ret = new String(buf.array());
            }

            //remove NUL character from conversion...
            ret = ret.replaceAll("\\u0000", "");
        } catch (CharacterCodingException e) {
            e.printStackTrace();
        }

        return ret;
    }

    /**
     * Entferne verbotene Zeichen aus Dateiname.
     *
     * @param name Dateiname
     * @return Bereinigte Fassung
     */
    public static String replaceLeerDateiname(String name) {
        String ret = name;

        // zuerst die Ersetzungstabelle mit den Wünschen des Users
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_USE_REPLACETABLE))) {
            ret = Daten.mVReplaceList.replace(ret, false /*path*/);
        }

        // und wenn gewünscht: "NUR Ascii-Zeichen"
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ONLY_ASCII))) {
            return convertToASCIIEncoding(ret);
        } else {
            return convertToNativeEncoding(ret);
        }
    }

}
