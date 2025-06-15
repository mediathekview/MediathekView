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

import org.w3c.dom.Document;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Base class for the TTML parsers.
 */
public abstract class TimedTextMarkupLanguageParser implements AutoCloseable, ITtmlParser {

    protected final List<Subtitle> subtitleList = new ArrayList<>();
    protected final SimpleDateFormat ttmlFormat = new SimpleDateFormat("HH:mm:ss.SS");
    protected final String backgroundColor = "#000000C2";
    protected String color = "#FFFFFF";
    protected Document doc;

    /**
     * Return all parsed subtitles.
     *
     * @return an <b>immutable</b> list of subtitles.
     */
    public List<Subtitle> getSubtitleList() {
        return Collections.unmodifiableList(subtitleList);
    }

    protected DocumentBuilder getDocumentBuilder() throws ParserConfigurationException {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        dbf.setNamespaceAware(true);
        return dbf.newDocumentBuilder();
    }


    @Override
    public void close() {
        subtitleList.clear();
    }
}
