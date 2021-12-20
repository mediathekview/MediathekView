package mediathek.daten.blacklist;

import mediathek.tool.Filter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import java.util.function.BiConsumer;

public class BlacklistRule {

    public static final String TAG = "Blacklist";
    private static final Logger logger = LogManager.getLogger();
    private boolean patternTitle = true;
    private boolean patternThema = true;

    private String nr;
    private String sender;
    private String thema;
    private String titel;
    private String thema_titel;

    public BlacklistRule() {
        nr = "";
        sender = "";
        thema = "";
        titel = "";
        thema_titel = "";
    }

    public BlacklistRule(String sender, String thema, String titel, String themaTitel) {
        this();
        this.sender = sender;
        this.thema = thema;
        this.titel = titel;
        this.thema_titel = themaTitel;
    }

    public @NotNull String getNr() {
        return nr;
    }

    public void setNr(@NotNull String s) {
        nr = s;
    }

    public @NotNull String getThemaTitel() {
        return thema_titel;
    }

    public void setThemaTitel(@NotNull String s) {
        thema_titel = s;
    }

    public @NotNull String getSender() {
        return sender;
    }

    public void setSender(@NotNull String s) {
        sender = s;
    }

    public @NotNull String getThema() {
        return thema;
    }

    public void setThema(@NotNull String s) {
        thema = s;
    }

    public @NotNull String getTitel() {
        return titel;
    }

    public void setTitel(@NotNull String s) {
        titel = s;
    }

    /**
     * Determine if we have regexp patterns somewhere and also precompile the pattern into the cache to speed up
     * operations a bit.
     */
    public void checkPatterns() {
        patternTitle = Filter.isPattern(titel);
        patternThema = Filter.isPattern(thema_titel);

        //precompile and cache the regexp patterns if needed...
        if (patternTitle)
            Filter.makePattern(titel);

        if (patternThema)
            Filter.makePattern(thema_titel);
    }

    public boolean hasTitlePattern() {
        return patternTitle;
    }

    public boolean hasThemaPattern() {
        return patternThema;
    }

    public void convertToLowerCase() {
        titel = titel.toLowerCase();
        thema_titel = thema_titel.toLowerCase();
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
