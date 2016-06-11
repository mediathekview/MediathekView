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

import mediathek.daten.Daten;

public class DebugMsg {

    public static synchronized void print(String text) {
        print_(new String[]{text});
    }

    public static synchronized void print(String[] text) {
        print_(text);
    }

    public static synchronized void printCl(String text) {
        printCl(new String[]{text});
    }

    public static synchronized void printCl(String[] text) {
        printCl_(text);
    }

    private static void printCl_(String[] texte) {
        final Throwable t = new Throwable();
        final StackTraceElement methodCaller = t.getStackTrace()[2];
        final String klasse = methodCaller.getClassName() + "." + methodCaller.getMethodName();
        String kl;
        try {
            kl = klasse;
            while (kl.contains(".")) {
                if (Character.isUpperCase(kl.charAt(0))) {
                    break;
                } else {
                    kl = kl.substring(kl.indexOf(".") + 1);
                }
            }
        } catch (Exception ignored) {
            kl = klasse;
        }

        if (Daten.debug) {
            final String z = "||";
            System.out.println(z + " " + kl);
            for (String text : texte) {
                System.out.println(z + "      " + text);
            }
            System.out.println("");
        }
    }

    private static void print_(String[] texte) {
        if (Daten.debug) {
            final String z = "||";
            for (String text : texte) {
                System.out.println(z + "  " + text);
            }
        }
    }

}
