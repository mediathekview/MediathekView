/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.mac;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Spotlight {
    /**
     * Perform a Spotlight search.
     *
     * @param query the query string to search for
     * @return a list of all files and folders matching the search
     */
    public static List<File> find(String query) throws IOException {
        return doSearch(new String[]{"mdfind", query});
    }


    /**
     * Perform a Spotlight search.
     *
     * @param query  the query string to search for
     * @param folder the search will be restricted to files inside this folder
     * @return a list of all files and folders matching the search
     */
    public static List<File> find(String query, File folder) throws IOException {
        return doSearch(new String[]{"mdfind", "-onlyin", folder.getAbsolutePath(), query});
    }


    private static List<File> doSearch(String[] command) throws IOException {
        var process = Runtime.getRuntime().exec(command);
        ArrayList<File> results = new ArrayList<>();
        try (var is = process.getInputStream();
             var isr = new InputStreamReader(is);
             var out = new BufferedReader(isr)) {
            String line;

            while ((line = out.readLine()) != null)
                results.add(new File(line));
        }

        try {
            process.waitFor();
        } catch (InterruptedException ignored) {
        }

        if (process.isAlive())
            process.destroy();

        return results;
    }


    /**
     * Get a map containing all searchable metadata attributes for a particular
     * file or folder.
     *
     * @param file the file to report on
     * @return a Map containing all metadata for the file
     */
    public static Map<String, String> getMetadata(File file) throws IOException {
        var process = Runtime.getRuntime().exec(new String[]{"/usr/bin/mdls", file.getAbsolutePath()});

        HashMap<String, String> results = new HashMap<>();
        try (var is = process.getInputStream();
             var isr = new InputStreamReader(is);
             var out = new BufferedReader(isr)) {
            String line;

            while ((line = out.readLine()) != null) {
                final int equals = line.indexOf('=');
                if (equals > -1) {
                    String key = line.substring(0, equals).trim();
                    String value = line.substring(equals + 1).trim();
                    results.put(key, value);
                }
            }
        }

        try {
            process.waitFor();
        } catch (InterruptedException ignored) {
        }

        if (process.isAlive())
            process.destroy();

        return results;
    }
}