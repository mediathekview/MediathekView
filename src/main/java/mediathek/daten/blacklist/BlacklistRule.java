package mediathek.daten.blacklist;

import mediathek.tool.Filter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import java.util.Arrays;
import java.util.function.BiConsumer;

public class BlacklistRule {

    public static final int MAX_ELEM = 5;
    public static final String TAG = "Blacklist";
    public static final String[] XML_NAMES = {"black-nr", "black-sender", "black-thema", "black-titel", "black-thema-titel"};
    private static final int BLACKLIST_NR = 0;
    private static final int BLACKLIST_THEMA_TITEL = 4;
    private static final int BLACKLIST_TITEL = 3;
    private static final int BLACKLIST_THEMA = 2;
    private static final int BLACKLIST_SENDER = 1;
    private static final Logger logger = LogManager.getLogger();
    public String[] arr;
    private boolean patternTitle = true;
    private boolean patternThema = true;

    public BlacklistRule() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr, "");
    }

    public BlacklistRule(String sender, String thema, String titel, String themaTitel) {
        this();

        arr[BLACKLIST_SENDER] = sender;
        arr[BLACKLIST_THEMA] = thema;
        arr[BLACKLIST_TITEL] = titel;
        arr[BLACKLIST_THEMA_TITEL] = themaTitel;
    }

    public @NotNull String getNr() {
        return arr[BLACKLIST_NR];
    }

    public void setNr(@NotNull String s) {
        arr[BLACKLIST_NR] = s;
    }

    public @NotNull String getThemaTitel() {
        return arr[BLACKLIST_THEMA_TITEL];
    }

    public void setThemaTitel(@NotNull String s) {
        arr[BLACKLIST_THEMA_TITEL] = s;
    }

    public @NotNull String getSender() {
        return arr[BLACKLIST_SENDER];
    }

    public void setSender(@NotNull String s) {
        arr[BLACKLIST_SENDER] = s;
    }

    public @NotNull String getThema() {
        return arr[BLACKLIST_THEMA];
    }

    public void setThema(@NotNull String s) {
        arr[BLACKLIST_THEMA] = s;
    }

    public @NotNull String getTitel() {
        return arr[BLACKLIST_TITEL];
    }

    public void setTitel(@NotNull String s) {
        arr[BLACKLIST_TITEL] = s;
    }

    /**
     * Determine if we have regexp patterns somewhere and also precompile the pattern into the cache to speed up
     * operations a bit.
     */
    public void checkPatterns() {
        patternTitle = Filter.isPattern(arr[BLACKLIST_TITEL]);
        patternThema = Filter.isPattern(arr[BLACKLIST_THEMA_TITEL]);

        //precompile and cache the regexp patterns if needed...
        if (patternTitle)
            Filter.makePattern(arr[BLACKLIST_TITEL]);

        if (patternThema)
            Filter.makePattern(arr[BLACKLIST_THEMA_TITEL]);
    }

    public boolean hasTitlePattern() {
        return patternTitle;
    }

    public boolean hasThemaPattern() {
        return patternThema;
    }

    public void convertToLowerCase() {
        arr[BLACKLIST_TITEL] = arr[BLACKLIST_TITEL].toLowerCase();
        arr[BLACKLIST_THEMA_TITEL] = arr[BLACKLIST_THEMA_TITEL].toLowerCase();
    }

    /**
     * Write all data to config.
     *
     * @param writer the writer used.
     */
    public void writeToConfig(@NotNull XMLStreamWriter writer) {
        final BiConsumer<String, String> writeElement = (tagName, content) -> {
            try {
                writer.writeCharacters("\t");
                writer.writeStartElement(tagName);
                writer.writeCharacters(content);
                writer.writeEndElement();
                writer.writeCharacters("\n");
            } catch (XMLStreamException e) {
                logger.error("writeElement failed", e);
            }
        };

        try {
            writer.writeStartElement(TAG);
            writer.writeCharacters("\n");


            writeElement.accept(BlacklistTags.NR.getXmlName(), getNr());
            writeElement.accept(BlacklistTags.SENDER.getXmlName(), getSender());
            writeElement.accept(BlacklistTags.THEMA.getXmlName(), getThema());
            writeElement.accept(BlacklistTags.TITEL.getXmlName(), getTitel());
            writeElement.accept(BlacklistTags.THEMA_TITEL.getXmlName(), getThemaTitel());

            writer.writeEndElement();
            writer.writeCharacters("\n");
        } catch (Exception ex) {
            logger.error("writeToConfig", ex);
        }
    }

    /**
     * Read data from config.
     *
     * @param parser the xml file parser.
     */
    public void readFromConfig(@NotNull XMLStreamReader parser) throws XMLStreamException, AssertionError {
        while (parser.hasNext()) {
            final int event = parser.next();
            if (event == XMLStreamConstants.END_ELEMENT) {
                if (parser.getLocalName().equals(TAG)) {
                    break;
                }
            }
            if (event == XMLStreamConstants.START_ELEMENT) {
                BlacklistTags.fromXmlTag(parser.getLocalName()).ifPresent(tag -> {
                    try {
                        final var text = parser.getElementText();
                        switch (tag) {
                            case NR -> setNr(text);
                            case SENDER -> setSender(text);
                            case THEMA -> setThema(text);
                            case TITEL -> setTitel(text);
                            case THEMA_TITEL -> setThemaTitel(text);
                            default -> throw new AssertionError("Illegal tag detected");
                        }
                    } catch (XMLStreamException e) {
                        logger.error("Error reading blacklist rule entry", e);
                    }
                });
            }
        }
    }
}
