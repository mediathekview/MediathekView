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

package mediathek.tool.ttml;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Subtitle {
    /**
     * The styled text lines of the subtitle.
     */
    private final List<StyledString> listOfStrings = new ArrayList<>();
    /**
     * The start time of the subtitle.
     */
    private Date begin;
    /**
     * The end time of the subtitle.
     */
    private Date end;
    /**
     * The region identifier for positioning the subtitle.
     */
    private String region;

    /**
     * Default constructor.
     */
    public Subtitle() {
    }

    /**
     * Constructs a Subtitle with specified times and region.
     *
     * @param begin  start time
     * @param end    end time
     * @param region positioning region
     */
    public Subtitle(Date begin, Date end, String region) {
        this.begin = begin;
        this.end = end;
        this.region = region;
    }

    // Optional getters and setters

    public Date getBegin() {
        return begin;
    }

    public void setBegin(Date begin) {
        this.begin = begin;
    }

    public Date getEnd() {
        return end;
    }

    public void setEnd(Date end) {
        this.end = end;
    }

    public List<StyledString> getListOfStrings() {
        return listOfStrings;
    }

    public String getRegion() {
        return region;
    }

    public void setRegion(String region) {
        this.region = region;
    }
}
