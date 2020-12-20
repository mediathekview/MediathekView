package mediathek.config;

import mediathek.tool.Log;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A enum which contains log messages.
 */
public enum Messages {

    ERROR_CANT_CREATE_FOLDER(Types.ERROR, "Der Ordner \"%s\" konnte nicht angelegt werden.%n Bitte prüfen Sie die " +
            "Dateirechte.");

    private static final Logger logger = LogManager.getLogger();
    private final Types messageType;
    private final String textPattern;
    private final Integer errorCode;

    Messages(final Types aMessageType, final String aTextPattern) {
        this(aMessageType, aTextPattern, null);
    }

    Messages(final Types aMessageType, final String aTextPattern, Integer aErrorCode) {
        messageType = aMessageType;
        textPattern = aTextPattern;
        errorCode = aErrorCode;
    }

    public static void logMessage(final Messages aMessage, final Exception aException, final Object... aFormattingArguments) {
        String message = aFormattingArguments == null ? aMessage.getText() : aMessage.getTextFormatted(aFormattingArguments);
        switch (aMessage.getMessageType()) {
            case ERROR:
                if (aException == null) {
                    logger.error("Error code: {}, Message: {}",
                            aMessage.getErrorCode() == null ? 0 : aMessage.getErrorCode(),
                            message);
                } else {
                    logger.error("Error code: {}, Message: {}",
                            aMessage.getErrorCode() == null ? 0 : aMessage.getErrorCode(),
                            message);
                    logger.error(aException);
                }
                break;
            case WARNING:
                Log.sysLog(aMessage.getMessageType().toString() + ": " + message);
                break;
            case INFO:
                Log.sysLog(message);
        }
    }

    public Integer getErrorCode() {
        return errorCode;
    }

    public Types getMessageType() {
        return messageType;
    }

    public String getTextFormatted(Object... aFormattingArguments) {
        return String.format(textPattern, aFormattingArguments);
    }

    public String getText() {
        return textPattern;
    }

    public enum Types {
        ERROR, WARNING, INFO
    }
}
