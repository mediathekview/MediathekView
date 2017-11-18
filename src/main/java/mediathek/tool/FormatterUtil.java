package mediathek.tool;


import org.apache.commons.lang3.time.FastDateFormat;

/**
 * Central collection class for used string formatters.
 * Since {@link FastDateFormat} is threadsafe we can use it this way.
 */
public final class FormatterUtil {
    public static final FastDateFormat FORMATTER_ddMMyyyy = FastDateFormat.getInstance("dd.MM.yyyy");
    public static final FastDateFormat FORMATTER_yyyyMMdd = FastDateFormat.getInstance("yyyyMMdd");
    public static final FastDateFormat FORMATTER_HHmmss = FastDateFormat.getInstance("HH:mm:ss");
    public static final FastDateFormat FORMATTER_ddMMyyyyHHmm = FastDateFormat.getInstance("dd.MM.yyyy, HH:mm");
    public static final FastDateFormat FORMATTER_ddMMyyyyHHmmss = FastDateFormat.getInstance("dd.MM.yyyyHH:mm:ss");
}
