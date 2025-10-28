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

package mediathek.tool.ttml.parsers;

import mediathek.tool.ttml.ITtmlParser;
import mediathek.tool.ttml.StyledString;
import mediathek.tool.ttml.Subtitle;
import mediathek.tool.ttml.TimedTextMarkupLanguageParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.nio.file.Path;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class XmlTimedTextMarkupLanguageParser extends TimedTextMarkupLanguageParser implements ITtmlParser {
    private static final Logger logger = LogManager.getLogger();
    private final SimpleDateFormat sdfFlash = new SimpleDateFormat("s.S");

    private Date parseFlash(String tStamp) throws ParseException {
        Date da;
        if (tStamp.contains(":")) {
            da = ttmlFormat.parse(tStamp);
        }
        else {
            da = sdfFlash.parse(tStamp + "00");
        }
        return da;
    }

    /**
     * Build the Subtitle objects from TTML content.
     */
    private void buildFilmListFlash() throws Exception {
        final NodeList subtitleData = doc.getElementsByTagName("p");

        for (int i = 0; i < subtitleData.getLength(); i++) {
            final Subtitle subtitle = new Subtitle();

            final Node subnode = subtitleData.item(i);
            if (subnode.hasAttributes()) {
                // retrieve the begin and end attributes
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node beginNode = attrMap.getNamedItem("begin");
                final Node endNode = attrMap.getNamedItem("end");
                if (beginNode != null && endNode != null) {
                    subtitle.setBegin(parseFlash(beginNode.getNodeValue()));
                    subtitle.setEnd(parseFlash(endNode.getNodeValue()));
                    final StyledString textContent =
                            new StyledString(subnode.getTextContent(), color, backgroundColor);

                    final Node col = attrMap.getNamedItem("tts:color");
                    if (col != null) {
                        textContent.setColor(col.getNodeValue());
                    }
                    else {
                        final NodeList childNodes = subnode.getChildNodes();
                        for (int j = 0; j < childNodes.getLength(); j++) {
                            final Node node = childNodes.item(j);
                            if (node.getNodeName().equalsIgnoreCase("span")) {
                                // retrieve the text and color information
                                final NamedNodeMap attr = node.getAttributes();
                                final Node co = attr.getNamedItem("tts:color");
                                textContent.setColor(co.getNodeValue());
                            }
                        }
                    }
                    subtitle.getListOfStrings().add(textContent);
                }
            }
            subtitleList.add(subtitle);
        }
    }

    /**
     * Parse the XML Subtitle File for Flash Player into internal representation.
     *
     * @param ttmlFilePath the TTML file to parse
     * @return true if successful
     */
    @Override
    public boolean parse(Path ttmlFilePath) {
        logger.trace("Parsing TTML file with Flash parser");
        boolean ret;
        try {
            doc = getDocumentBuilder().parse(ttmlFilePath.toFile());

            // Check that we have TTML v1.0 file as we have tested only them.
            final NodeList metaData = doc.getElementsByTagName("tt");
            final NodeList colorNote = doc.getElementsByTagName("style");
            if (metaData != null) {
                final Node node = metaData.item(0);

                if (node.hasAttributes()) {
                    // retrieve the begin and end attributes
                    final NamedNodeMap attrMap = node.getAttributes();
                    final Node xmlns = attrMap.getNamedItem("xmlns");
                    if (xmlns != null) {
                        final String s = xmlns.getNodeValue();
                        if (!s.equals("http://www.w3.org/2006/04/ttaf1")
                                && !s.equals("http://www.w3.org/ns/ttml")) {
                            logger.warn("Invalid namespace.");
                            return false;
                        }
                    }
                }
                else {
                    logger.warn("Missing attributes.");
                    return false;
                }
            }
            else {
                logger.warn("Missing metadata tag.");
                return false;
            }
            if (colorNote != null) {
                if (colorNote.getLength() == 0) {
                    this.color = "#FFFFFF";
                }
                else {
                    final Node node = colorNote.item(0);

                    if (node.hasAttributes()) {
                        // retrieve the begin and end attributes
                        final NamedNodeMap attrMap = node.getAttributes();
                        final Node col = attrMap.getNamedItem("tts:color");
                        if (col != null) {
                            if (!col.getNodeValue().isEmpty()) {
                                this.color = col.getNodeValue();
                            }
                        }
                    }
                    else {
                        throw new Exception("Unknown File Format");
                    }
                }
            }
            else {
                throw new Exception("Unknown File Format");
            }
            buildFilmListFlash();
            ret = true;
        }
        catch (Exception ex) {
            logger.error("File: {}", ttmlFilePath, ex);
            ret = false;
        }
        return ret;
    }
}
