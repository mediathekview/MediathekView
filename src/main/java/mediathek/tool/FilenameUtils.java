package mediathek.tool;

import com.ibm.icu.text.Transliterator;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.*;

/**
 * User: crystalpalace1977
 * Date: 28.12.14
 * Time: 16:02
 */
public class FilenameUtils {

    /**
     * Valid characters for Windows in file names:
     * Based on <a href="http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx">MSDN sample</a>
     */
    public static final String REGEXP_ILLEGAL_CHARACTERS_WINDOWS = "[:\\\\/*?|<>\"]";
    public static final String REGEXP_ILLEGAL_CHARACTERS_WINDOWS_PATH = "[:/*?|<>\"]";

    /**
     * Valid characters for all UNIX-like OS.
     */
    public static final String REGEXP_ILLEGAL_CHARACTERS_OTHERS = "[:\\\\/*|<>]";
    public static final String REGEXP_ILLEGAL_CHARACTERS_OTHERS_PATH = "[:\\\\*|<>]";
    private static final Logger logger = LogManager.getLogger();

    public static String checkFilenameForIllegalCharacters(final String name, final boolean isPath) {
        // dient nur zur Anzeige für Probleme (Textfeld wird rot)
        String ret = name;
        boolean isWindowsPath = false;
        final String splitChar;

        if (SystemUtils.IS_OS_WINDOWS) {
            splitChar = "\\\\";
        } else {
            splitChar = File.separator;
        }

        if (SystemUtils.IS_OS_WINDOWS) {
            ret = removeWindowsTrailingDots(ret);
            if (isPath) {
                if (ret.length() > 1 && ret.charAt(1) == ':') {
                    // damit auch "d:" und nicht nur "d:\" als Pfad geht
                    isWindowsPath = true;
                    ret = ret.replaceFirst(":", ""); // muss zum Schluss wieder rein, kann aber so nicht ersetzt werden
                }
            }
        } else {
            ret = stripStartingDots(ret);
        }

        if (isPath && ret.contains(File.separator)) {
            String str = "";
            final String[] sa = ret.split(splitChar); // Regex
            for (String s : sa) {
                if (!s.isEmpty()) {
                    str += File.separator + convertToNativeEncoding(s, false); //sind ja nur noch die Ordnernamen
                }
            }
            if (!ret.startsWith(File.separator)) {
                str = str.replaceFirst(splitChar, ""); // wieder Regex
            }
            if (ret.endsWith(File.separator)) {
                str += File.separator;
            }
            ret = str;
        } else {
            ret = convertToNativeEncoding(ret, false);
        }

        if (isWindowsPath) {
            // c: wieder herstellen
            if (ret.length() == 1) {
                ret += ":";
            } else if (ret.length() > 1) {
                ret = ret.charAt(0) + ":" + ret.substring(1);
            }
        }

        return ret;
    }

    /**
     * Remove all starting dots from a string, <b>if</b> it begins with them.
     * @param input the input string
     * @return the stripped result
     */
    protected static String stripStartingDots(@NotNull String input) {
        return input.replaceFirst("^\\.+", "");
    }

    /**
     * Remove stray trailing dots from string when we are on Windows OS.
     *
     * @param fileName A filename string that might include trailing dots.
     * @return Cleanup string with no dots anymore.
     */
    protected static String removeWindowsTrailingDots(String fileName) {
        // machte unter Win noch Probleme, zB. bei dem Titel: "betrifft: ..."
        // "." und " " am Ende machen Probleme
        while (!fileName.isEmpty() && (fileName.endsWith(".") || fileName.endsWith(" "))) {
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
    private static String convertToNativeEncoding(String fileName, boolean isPath) {
        String ret = fileName;

        ret = removeIllegalCharacters(ret, isPath);

        //convert our filename to OS encoding...
        try {
            final CharsetEncoder charsetEncoder = Charset.defaultCharset().newEncoder();
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
            logger.error("convertToNativeEncoding", e);
        }

        return ret;
    }

    /**
     * Convert a filename from Java´s native UTF-16 to US-ASCII character encoding.
     *
     * @param fileName The UTF-16 filename string.
     * @return US-ASCII encoded string for the OS.
     */
    protected static String convertToASCIIEncoding(String fileName, boolean isPath) {
        String ret = fileName;
        //remove NUL character from conversion...
        ret = ret.replaceAll("\\u0000", "");

        //convert to ASCII with icu4j
        var transliterator = Transliterator.getInstance("de-ASCII");
        ret = transliterator.transliterate(ret);

        ret = removeIllegalCharacters(ret, isPath);

        //convert our filename to OS encoding...
        try {
            final CharsetEncoder charsetEncoder = StandardCharsets.US_ASCII.newEncoder();
            charsetEncoder.onMalformedInput(CodingErrorAction.REPLACE); // otherwise breaks on first unconvertable char
            charsetEncoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
            charsetEncoder.replaceWith(new byte[]{'_'});

            final ByteBuffer buf = charsetEncoder.encode(CharBuffer.wrap(ret));
            if (buf.hasArray()) {
                ret = new String(buf.array());
            }
        } catch (CharacterCodingException e) {
            logger.error("convertToASCIIEncoding", e);
        }

        return ret;
    }

    /**
     * Remove illegal characters from String based on current OS.
     *
     * @param input  The input string
     * @param isPath true if this is a path.
     * @return Cleaned-up string.
     */
    public static String removeIllegalCharacters(final String input, boolean isPath) {
        String ret = input;

        if (SystemUtils.IS_OS_WINDOWS) {
            //we need to be more careful on Windows when using e.g. FAT32
            //Therefore be more conservative by default and replace more characters.
            ret = removeWindowsTrailingDots(ret);
            ret = ret.replaceAll(isPath ? REGEXP_ILLEGAL_CHARACTERS_WINDOWS_PATH : REGEXP_ILLEGAL_CHARACTERS_WINDOWS, "_");
        } else if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_MAC_OSX) {
            //On OSX the VFS take care of writing correct filenames to FAT filesystems...
            //Just remove the default illegal characters
            ret = stripStartingDots(ret);
            ret = ret.replaceAll(isPath ? REGEXP_ILLEGAL_CHARACTERS_OTHERS_PATH : REGEXP_ILLEGAL_CHARACTERS_OTHERS, "_");
        } else {
            throw new IllegalStateException("Unsupported OS: " + SystemUtils.OS_NAME);
        }

        return ret;
    }

    /**
     * Entferne verbotene Zeichen aus Dateiname.
     *
     * @param name Dateiname
     * @return Bereinigte Fassung
     */
    public static String replaceLeerDateiname(String name, boolean isPath, boolean userReplace, boolean onlyAscii) {
        String ret = name;
        boolean isWindowsPath = false;
        if (SystemUtils.IS_OS_WINDOWS && isPath && ret.length() > 1 && ret.charAt(1) == ':') {
            // damit auch "d:" und nicht nur "d:\" als Pfad geht
            isWindowsPath = true;
            ret = ret.replaceFirst(":", ""); // muss zum Schluss wieder rein, kann aber so nicht ersetzt werden
        }

        // zuerst die Ersetzungstabelle mit den Wünschen des Users
        if (userReplace) {
            ret = ReplaceList.replace(ret, isPath);
        }

        // und wenn gewünscht: "NUR Ascii-Zeichen"
        if (onlyAscii) {
            ret = convertToASCIIEncoding(ret, isPath);
        } else {
            ret = convertToNativeEncoding(ret, isPath);
        }

        if (isWindowsPath) {
            // c: wieder herstellen
            if (ret.length() == 1) {
                ret += ":";
            } else if (ret.length() > 1) {
                ret = ret.charAt(0) + ":" + ret.substring(1);
            }
        }
        return ret;
    }

}
