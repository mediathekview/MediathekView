package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

public class MVFilmSize implements Comparable<MVFilmSize> {

    private long aktSizeL = -1L;
    private Long sizeL = 0L;
    private static final Logger logger = LogManager.getLogger(MVFilmSize.class);

    public MVFilmSize() {
    }

    public static String getGroesse(long l) {
        String ret = "";
        if (l > 1_000_000) {
            // größer als 1MB sonst kann ich mirs sparen
            ret = String.valueOf(l / 1_000_000);
        } else if (l > 0) {
            ret = "1";
        }
        return ret;
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

    public void setSize(String size) {
        // im Film ist die Größe in "MB" !!
        if (size.isEmpty()) {
            aktSizeL = -1L;
            sizeL = 0L;
        } else {
            try {
                sizeL = Long.parseLong(size) * 1_000_000;
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
                sizeStr = getGroesse(sizeL);
            } else {
                sizeStr = "";
            }
        } else if (sizeL > 0) {
            sizeStr = getGroesse(aktSizeL) + " von " + getGroesse(sizeL);
        } else {
            sizeStr = getGroesse(aktSizeL);
        }

        return sizeStr;
    }
}
