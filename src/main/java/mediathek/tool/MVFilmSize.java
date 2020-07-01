package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

/**
 * Store sizes in bytes but return strings in MegaBytes.
 */
public class MVFilmSize implements Comparable<MVFilmSize> {

    private long aktSizeL = -1L;
    private Long sizeL = 0L;
    private static final Logger logger = LogManager.getLogger();

    public MVFilmSize() {
    }

    @Override
    public int compareTo(@NotNull MVFilmSize ll) {
        return (sizeL.compareTo(ll.sizeL));
    }

    @Override
    public String toString() {
        return prepareString();
    }

    public void reset() {
        aktSizeL = -1L;
    }

    public long getSize() {
        return sizeL;
    }

    /**
     * Store filmsize in bytes.
     * Converts the MEGABYTES to byte.
     *
     * @param size String of filmsize in MEGABYTE
     */
    public void setSize(String size) {
        // im Film ist die Größe in "MB" !!
        if (size.isEmpty()) {
            aktSizeL = -1L;
            sizeL = 0L;
        } else {
            try {
                sizeL = Long.parseLong(size) * FileSize.ONE_MiB;
            } catch (Exception ex) {
                logger.error("string: {}, ex: {}", size, ex);
                sizeL = 0L;
            }
        }
    }

    public void setSize(long l) {
        sizeL = l;
    }

    public void addAktSize(long l) {
        aktSizeL += l;
    }

    public long getAktSize() {
        return aktSizeL;
    }

    public void setAktSize(long l) {
        aktSizeL = l;
    }

    private String prepareString() {
        String sizeStr;

        if (aktSizeL <= 0) {
            if (sizeL > 0) {
                sizeStr = FileSize.convertSize(sizeL);
            } else {
                sizeStr = "";
            }
        } else if (sizeL > 0) {
            sizeStr = FileSize.convertSize(aktSizeL) + " von " + FileSize.convertSize(sizeL);
        } else {
            sizeStr = FileSize.convertSize(aktSizeL);
        }

        return sizeStr;
    }
}
