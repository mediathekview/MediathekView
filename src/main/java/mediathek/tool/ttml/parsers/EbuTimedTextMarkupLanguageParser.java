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
import org.jetbrains.annotations.NotNull;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.nio.file.Path;
import java.util.*;

/**
 * Parser for EBU TTML XML subtitle files.
 * Tested with TTML format version 1.0.
 */
public class EbuTimedTextMarkupLanguageParser extends TimedTextMarkupLanguageParser implements ITtmlParser {
    private static final Logger logger = LogManager.getLogger();
    private final Map<String, Integer> regionMap = new HashMap<>();
    private final Map<String, Integer> alignMap = new HashMap<>();
    private final Map<String, List<String>> colorMap = new HashMap<>();

    /**
     * Convert the text alignment to int:
     * <p>start: -1
     *
     * <p>left: -1
     *
     * <p>center: 0
     *
     * <p>right: 1
     *
     * <p>end: 1
     *
     * @param text the text representation
     * @return an int of the text alignment.
     */
    protected int convertAlignment(@NotNull String text) {
        return switch (text) {
            case "start", "left" -> -1;
            case "right", "end" -> 1;
            default -> 0;
        };
    }

    /**
     * Convert the region text to int value.
     *
     * <p>ASS format aligns to numbers as seen on the numpad on a keyboard.
     * <br/>Possible positions:
     * <br/>7 <b>8</b> 9
     * <br/>4 <b>5</b> 6
     * <br/>1 <b>2</b> 3
     *
     * <p>Defaults to bottom center (2).
     *
     * @param text the text value.
     * @return the int value.
     */
    protected int convertRegion(@NotNull String text) {
        return switch (text) {
            case "before" -> 8;
            case "center" -> 5;
            default -> 2;
        };
    }

    /**
     * Parse the TTML file into internal representation.
     *
     * @param ttmlFilePath the TTML file to parse
     */
    @Override
    public boolean parse(Path ttmlFilePath) {
        logger.trace("Parsing TTML file with EBU parser");
        boolean ret;
        try {
            doc = getDocumentBuilder().parse(ttmlFilePath.toFile());

            // Check that we have TTML v1.0 file as we have tested only them
            final NodeList metaData = doc.getElementsByTagName("ebuttm:documentEbuttVersion");
            if (metaData != null) {
                final Node versionNode = metaData.item(0);
                if (versionNode == null || !versionNode.getTextContent().equalsIgnoreCase("v1.0")) {
                    logger.warn("EBU version not found or incorrect version.");
                    return false;
                }
            }
            else {
                logger.warn("EBU document version tag not found.");
                return false;
            }

            buildAlignmentMap();
            buildColorMap();
            buildRegionMap();
            buildFilmList();
            ret = true;
        }
        catch (Exception ex) {
            logger.error("File: {}", ttmlFilePath, ex);
            ret = false;
        }
        return ret;
    }

    /**
     * Build a map of used screen regions within the TTML file.
     *
     * <p>This ignores the tts:origin and tts:extent attributes for now and solely decides position
     * mapping on tts:displayAlign.
     */
    private void buildRegionMap() {
        final NodeList styleData = doc.getElementsByTagName("tt:region");
        for (int i = 0; i < styleData.getLength(); i++) {
            final Node subnode = styleData.item(i);
            if (subnode.hasAttributes()) {
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node idNode = attrMap.getNamedItem("xml:id");
                final Node regionNode = attrMap.getNamedItem("tts:displayAlign");
                if (idNode != null && regionNode != null) {
                    final var region = convertRegion(regionNode.getNodeValue());
                    regionMap.put(idNode.getNodeValue(), region);
                }
            }
        }
    }

    /**
     * Build the Subtitle objects from TTML content.
     */
    private void buildFilmList() throws Exception {
        final NodeList subtitleData = doc.getElementsByTagName("tt:p");

        for (int i = 0; i < subtitleData.getLength(); i++) {
            final Subtitle subtitle = new Subtitle();

            final Node subnode = subtitleData.item(i);
            if (subnode.hasAttributes()) {
                // retrieve the begin and end attributes
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node beginNode = attrMap.getNamedItem("begin");
                final Node endNode = attrMap.getNamedItem("end");
                if (beginNode != null && endNode != null) {
                    subtitle.setBegin(ttmlFormat.parse(beginNode.getNodeValue()));
                    checkHours(subtitle.getBegin());

                    subtitle.setEnd(ttmlFormat.parse(endNode.getNodeValue()));
                    checkHours(subtitle.getEnd());
                }

                final Node regionNode = attrMap.getNamedItem("region");
                final Node styleNode = attrMap.getNamedItem("style");
                final Integer alignment =
                        (styleNode != null && alignMap.containsKey(styleNode.getNodeValue()))
                                ? alignMap.get(styleNode.getNodeValue())
                                : 0;
                final Integer region =
                        (regionNode != null && regionMap.containsKey(regionNode.getNodeValue()))
                                ? regionMap.get(regionNode.getNodeValue())
                                : 2;
                subtitle.setRegion(Integer.toString(alignment + region));
            }

            final NodeList childNodes = subnode.getChildNodes();
            for (int j = 0; j < childNodes.getLength(); j++) {
                final Node node = childNodes.item(j);
                if (node.getNodeName().equalsIgnoreCase("tt:span")) {
                    // retrieve the text and color information
                    final NamedNodeMap attrMap = node.getAttributes();
                    final Node styleNode = attrMap.getNamedItem("style");

                    final StyledString textContent = new StyledString();
                    textContent.setText(node.getTextContent());

                    final List<String> colors =
                            (styleNode != null && colorMap.containsKey(styleNode.getNodeValue()))
                                    ? colorMap.get(styleNode.getNodeValue())
                                    : null;
                    if (colors == null) {
                        textContent.setColor(color); // gabs beim BR
                        textContent.setBackgroundColor(backgroundColor);
                    }
                    else {
                        textContent.setColor(colors.get(0));
                        textContent.setBackgroundColor(colors.get(1));
                    }
                    subtitle.getListOfStrings().add(textContent);
                }
            }
            subtitleList.add(subtitle);
        }
    }

    @Override
    public void close() {
        super.close();
        colorMap.clear();
        regionMap.clear();
        alignMap.clear();
    }

    private void checkHours(@NotNull Date date) {
        // HACK:: Don´t know why this is set like this.
        // but we have to subract 10 hours from the XML
        final int hours = date.getHours();
        if (hours >= 10) {
            date.setHours(hours - 10);
        }
    }

    /**
     * Build a map of used colors within the TTML file.
     */
    private void buildColorMap() {
        final NodeList styleData = doc.getElementsByTagName("tt:style");
        for (int i = 0; i < styleData.getLength(); i++) {
            final Node subnode = styleData.item(i);
            if (subnode.hasAttributes()) {
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node idNode = attrMap.getNamedItem("xml:id");
                final Node colorNode = attrMap.getNamedItem("tts:color");
                final Node colorBackgroundNode = attrMap.getNamedItem("tts:backgroundColor");
                if (idNode != null && colorNode != null) {
                    final List<String> colorList = new ArrayList<>();
                    colorList.add(colorNode.getNodeValue());
                    if (colorBackgroundNode != null) {
                        colorList.add(colorBackgroundNode.getNodeValue());
                    }
                    else {
                        colorList.add(backgroundColor);
                    }
                    colorMap.put(idNode.getNodeValue(), colorList);
                }
            }
        }
    }

    /**
     * Build a map of used alignments within the TTML file.
     */
    private void buildAlignmentMap() {
        final NodeList styleData = doc.getElementsByTagName("tt:style");
        for (int i = 0; i < styleData.getLength(); i++) {
            final Node subnode = styleData.item(i);
            if (subnode.hasAttributes()) {
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node idNode = attrMap.getNamedItem("xml:id");
                final Node alignmentNode = attrMap.getNamedItem("tts:textAlign");
                if (idNode != null && alignmentNode != null) {
                    final int alignment = convertAlignment(alignmentNode.getNodeValue());
                    alignMap.put(idNode.getNodeValue(), alignment);
                }
            }
        }
    }

}
