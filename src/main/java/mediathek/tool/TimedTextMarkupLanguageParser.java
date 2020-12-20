/*
 *    TimedTextMarkupLanguageParser
 *    Copyright (C) 2016 CrystalPalace
 *    crystalpalace1977@googlemail.com
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Converter for TTML XML subtitle files into SubRip Text format.
 * Tested with MediathekView downloaded subtitles and TTML format version 1.0.
 */
public class TimedTextMarkupLanguageParser implements AutoCloseable {

    private static final Logger logger = LogManager.getLogger(TimedTextMarkupLanguageParser.class);
    private final SimpleDateFormat ttmlFormat = new SimpleDateFormat("HH:mm:ss.SS");
    private final SimpleDateFormat srtFormat = new SimpleDateFormat("HH:mm:ss,SS");
    private final SimpleDateFormat sdfFlash = new SimpleDateFormat("s.S");
    private final Map<String, String> colorMap = new Hashtable<>();
    private final List<Subtitle> subtitleList = new ArrayList<>();
    private String color = "#FFFFFF";
    private Document doc;

    public TimedTextMarkupLanguageParser() {
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
                if (idNode != null && colorNode != null) {
                    colorMap.put(idNode.getNodeValue(), colorNode.getNodeValue());
                }
            }
        }
    }

    /**
     * Build the Subtitle objects from TTML content.
     */
    @SuppressWarnings("deprecation")
    private void buildFilmList() throws Exception {
        final NodeList subtitleData = doc.getElementsByTagName("tt:p");

        for (int i = 0; i < subtitleData.getLength(); i++) {
            final Subtitle subtitle = new Subtitle();

            final Node subnode = subtitleData.item(i);
            if (subnode.hasAttributes()) {
                // retrieve the begin and end attributes...
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node beginNode = attrMap.getNamedItem("begin");
                final Node endNode = attrMap.getNamedItem("end");
                if (beginNode != null && endNode != null) {
                    subtitle.begin = ttmlFormat.parse(beginNode.getNodeValue());
                    //HACK:: Don´t know why this is set like this...
                    //but we have to subract 10 hours from the XML
                    if (subtitle.begin.getHours() >= 10) {
                        subtitle.begin.setHours(subtitle.begin.getHours() - 10);
                    }
                    subtitle.end = ttmlFormat.parse(endNode.getNodeValue());
                    if (subtitle.end.getHours() >= 10) {
                        subtitle.end.setHours(subtitle.end.getHours() - 10);
                    }

                }
            }

            final NodeList childNodes = subnode.getChildNodes();
            for (int j = 0; j < childNodes.getLength(); j++) {
                final Node node = childNodes.item(j);
                if (node.getNodeName().equalsIgnoreCase("tt:span")) {
                    //retrieve the text and color information...
                    final NamedNodeMap attrMap = node.getAttributes();
                    final Node styleNode = attrMap.getNamedItem("style");
                    final StyledString textContent = new StyledString();

                    textContent.setText(node.getTextContent());
                    final String col = colorMap.get(styleNode.getNodeValue());
                    if (col == null) {
                        textContent.setColor(color); // gabs beim BR
                    } else {
                        textContent.setColor(colorMap.get(styleNode.getNodeValue()));
                    }
                    subtitle.listOfStrings.add(textContent);
                }
            }
            subtitleList.add(subtitle);
        }
    }

    private Date parseFlash(String tStamp) throws ParseException {
        Date da;
        if (tStamp.contains(":")) {
            da = ttmlFormat.parse(tStamp);
        } else {
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
                // retrieve the begin and end attributes...
                final NamedNodeMap attrMap = subnode.getAttributes();
                final Node beginNode = attrMap.getNamedItem("begin");
                final Node endNode = attrMap.getNamedItem("end");
                if (beginNode != null && endNode != null) {
                    subtitle.begin = parseFlash(beginNode.getNodeValue());
                    subtitle.end = parseFlash(endNode.getNodeValue());
                    final StyledString textContent = new StyledString();
                    textContent.setColor(color); // sicher ist sicher
                    textContent.setText(subnode.getTextContent());

                    final Node col = attrMap.getNamedItem("tts:color");
                    if (col != null) {
                        textContent.setColor(col.getNodeValue());
                    } else {
                        final NodeList childNodes = subnode.getChildNodes();
                        for (int j = 0; j < childNodes.getLength(); j++) {
                            final Node node = childNodes.item(j);
                            if (node.getNodeName().equalsIgnoreCase("span")) {
                                //retrieve the text and color information...
                                final NamedNodeMap attr = node.getAttributes();
                                final Node co = attr.getNamedItem("tts:color");
                                textContent.setColor(co.getNodeValue());
                            }
                        }
                    }
                    subtitle.listOfStrings.add(textContent);

                }
            }
            subtitleList.add(subtitle);
        }
    }

    /**
     * Parse the TTML file into internal representation.
     *
     * @param ttmlFilePath the TTML file to parse
     */
    public boolean parse(Path ttmlFilePath) {
        boolean ret;
        try {
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);

            final DocumentBuilder db = dbf.newDocumentBuilder();
            doc = db.parse(ttmlFilePath.toFile());

            //Check that we have TTML v1.0 file as we have tested only them...
            final NodeList metaData = doc.getElementsByTagName("ebuttm:documentEbuttVersion");
            if (metaData != null) {
                final Node versionNode = metaData.item(0);
                if (versionNode == null || !versionNode.getTextContent().equalsIgnoreCase("v1.0")) {
                    throw new Exception("Unknown TTML file version");
                }
            } else {
                throw new Exception("Unknown File Format");
            }

            buildColorMap();
            buildFilmList();
            ret = true;
        } catch (Exception ex) {
            logger.error("File: " + ttmlFilePath, ex);
            ret = false;
        }
        return ret;
    }

    /**
     * Parse the XML Subtitle File for Flash Player into internal representation.
     *
     * @param ttmlFilePath the TTML file to parse
     * @return true if successful
     */
    public boolean parseXmlFlash(Path ttmlFilePath) {
        boolean ret;
        try {
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);

            final DocumentBuilder db = dbf.newDocumentBuilder();
            doc = db.parse(ttmlFilePath.toFile());

            //Check that we have TTML v1.0 file as we have tested only them...
            final NodeList metaData = doc.getElementsByTagName("tt");
            final NodeList colorNote = doc.getElementsByTagName("style");
            if (metaData != null) {
                final Node node = metaData.item(0);

                if (node.hasAttributes()) {
                    // retrieve the begin and end attributes...
                    final NamedNodeMap attrMap = node.getAttributes();
                    final Node xmlns = attrMap.getNamedItem("xmlns");
                    if (xmlns != null) {
                        final String s = xmlns.getNodeValue();
                        if (!s.equals("http://www.w3.org/2006/04/ttaf1")
                                && !s.equals("http://www.w3.org/ns/ttml")) {
                            throw new Exception("Unknown TTML file version");
                        }
                    }
                } else {
                    throw new Exception("Unknown File Format");
                }
            } else {
                throw new Exception("Unknown File Format");
            }
            if (colorNote != null) {
                if (colorNote.getLength() == 0) {
                    this.color = "#FFFFFF";
                } else {
                    final Node node = colorNote.item(0);

                    if (node.hasAttributes()) {
                        // retrieve the begin and end attributes...
                        final NamedNodeMap attrMap = node.getAttributes();
                        final Node col = attrMap.getNamedItem("tts:color");
                        if (col != null) {
                            if (!col.getNodeValue().isEmpty()) {
                                this.color = col.getNodeValue();
                            }
                        }
                    } else {
                        throw new Exception("Unknown File Format");
                    }
                }
            } else {
                throw new Exception("Unknown File Format");
            }
            buildFilmListFlash();
            ret = true;
        } catch (Exception ex) {
            logger.error("File: " + ttmlFilePath,ex);
            ret = false;
        }
        return ret;
    }

    /**
     * Convert internal representation into SubRip Text Format and save to file.
     */
    public void toSrt(Path srtFile) {
        try (FileOutputStream fos = new FileOutputStream(srtFile.toFile());
             OutputStreamWriter osw = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
             PrintWriter writer = new PrintWriter(osw)) {
            long counter = 1;
            for (Subtitle title : subtitleList) {
                writer.println(counter);
                writer.println(srtFormat.format(title.begin) + " --> " + srtFormat.format(title.end));
                for (StyledString entry : title.listOfStrings) {
                    if (!entry.color.isEmpty()) {
                        writer.print("<font color=\"" + entry.color + "\">");
                    }
                    writer.print(entry.text);
                    if (!entry.color.isEmpty()) {
                        writer.print("</font>");
                    }
                    writer.println();
                }
                writer.println("");
                counter++;
            }
        } catch (Exception ex) {
            logger.error("File: " + srtFile, ex);
        }
    }

    @Override
    public void close() {
        colorMap.clear();
        subtitleList.clear();
    }

    private static class StyledString {

        private String text = "";
        private String color = "";

        public String getText() {
            return text;
        }

        public void setText(String text) {
            this.text = text;
        }

        public String getColor() {
            return color;
        }

        public void setColor(String color) {
            this.color = color;
        }
    }

    private static class Subtitle {

        public Date begin;
        public Date end;
        public List<StyledString> listOfStrings = new ArrayList<>();
    }
}
