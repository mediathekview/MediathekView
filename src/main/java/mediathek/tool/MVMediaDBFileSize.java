package mediathek.tool;

import org.jetbrains.annotations.NotNull;

public class MVMediaDBFileSize implements Comparable<MVMediaDBFileSize> {

    public Long sizeL = 0L;
    private String sizeStr = "";

    public MVMediaDBFileSize(long size) {
        sizeL = size;
        sizeStr = setGroesse(size);
    }

    @Override
    public int compareTo(@NotNull MVMediaDBFileSize ll) {
        return (sizeL.compareTo(ll.sizeL));
    }

    @Override
    public String toString() {
        return sizeStr;
    }

    private String setGroesse(long l) {
        // l: Anzahl Bytes
        String ret = "";
        if (l > 1000 * 1000) {
            // größer als 1MB sonst kann ich mirs sparen
            ret = String.valueOf(l / (1000 * 1000));
        } else if (l > 0) {
            //0<....<1M
            ret = "< 1";
        } else if (l == 0) {
            ret = "0";
        }
        return ret;
    }
}
