/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.tool;

public class MVFunction {

    /**
     * Convert a byte count into a human readable string.
     *
     * @param bytes The number of bytes to convert.
     * @param si Use International System of Units (SI)?
     * @return The string representation
     */
    public static String humanReadableByteCount(final long bytes, final boolean si) {
        final int unit = si ? 1000 : 1024;
        if (bytes < unit) {
            return bytes + " B";
        }

        final int exp = (int) (Math.log(bytes) / Math.log(unit));

        final String pre = (si ? "kMGTPE" : "KMGTPE").charAt(exp - 1) + (si ? "" : "i");
        return String.format("%.1f %sB", bytes / Math.pow(unit, exp), pre);
    }
//    public enum ReleaseType { DEBUG, RELEASE}
//    public static ReleaseType getReleaseType()
//    {
//        ReleaseType releaseType;
//        try {
//            ResourceBundle.clearCache();
//            ResourceBundle rb = ResourceBundle.getBundle("version");
//            String msg = rb.getString("TYPE");
//            switch(msg) {
//                case "DEBUG":
//                    releaseType = ReleaseType.DEBUG;
//                    break;
//                default:
//                    releaseType = ReleaseType.RELEASE;
//                    break;
//            }
//        } catch (Exception ex) {
//            //in case of an exception always pretend we are in Release mode...
//            ex.printStackTrace();
//            releaseType = ReleaseType.DEBUG;////////////////
//        }
//
//        return releaseType;
//    }

}
