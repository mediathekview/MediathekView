package mediathek.tool;

import static org.apache.commons.io.FileUtils.ONE_EB_BI;
import static org.apache.commons.io.FileUtils.ONE_GB_BI;
import static org.apache.commons.io.FileUtils.ONE_KB_BI;
import static org.apache.commons.io.FileUtils.ONE_MB_BI;
import static org.apache.commons.io.FileUtils.ONE_PB_BI;
import static org.apache.commons.io.FileUtils.ONE_TB_BI;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Custom Implementation of FileUtils.byteCountToDisplaySize to fix rounding bug
 * taken from https://issues.apache.org/jira/browse/IO-373 (2019-09-20)
 * post from Sammy Trojette, 2016-11-01-10-09
 * 
 * quote
 * Since this has just come up as an issue on our current project. I'd like to do a little necro here.
 * With this version, the displayed value is always set at least 3 numericals so: ####, ###, ##.# or #.##.
 * If anyone sees a serious issue, please give me a call.
 */
public class ByteUnitUtil {

    private static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_UP;

    enum FileSize {
        EXABYTE("EB", ONE_EB_BI),
        PETABYTE("PB", ONE_PB_BI),
        TERABYTE("TB", ONE_TB_BI),
        GIGABYTE("GB", ONE_GB_BI),
        MEGABYTE("MB", ONE_MB_BI),
        KILOBYTE("KB", ONE_KB_BI),
        BYTE("bytes", BigInteger.ONE);

        private final String unitName;
        private final BigInteger byteCount;

        FileSize(String unitName, BigInteger byteCount) {
            this.unitName = unitName;
            this.byteCount = byteCount;
        }
        private String unitName() {
            return unitName;
        }
        private BigInteger byteCount() {
            return byteCount;
        }

    }

    /**
     * Formats a file's size into a human readable format
     *
     * @param fileSize the file's size as BigInteger
     * @return the size as human readable string
     */
    public static String byteCountToDisplaySize(final BigInteger fileSize) {

        String unit = FileSize.BYTE.unitName;
        BigDecimal fileSizeInUnit = BigDecimal.ZERO;
        String val;

        for (FileSize fs : FileSize.values()) {
            BigDecimal size_bd = new BigDecimal(fileSize);
            fileSizeInUnit = size_bd.divide(new BigDecimal(fs.byteCount()), 5, ROUNDING_MODE);
            if (fileSizeInUnit.compareTo(BigDecimal.ONE) >= 0) {
                unit = fs.unitName();
                break;
            }
        }
        
        // always round so that at least 3 numerics are displayed (###, ##.#, #.##)
        if (fileSizeInUnit.compareTo(BigDecimal.valueOf(100.0)) >= 0) {
        	// with fileSizeInUnit with 3 or 4 digits (100, 101, ..., 999, 1000, ..., 1023) do not display any decimal places
            val = fileSizeInUnit.setScale(0, ROUNDING_MODE).toString();
        } else if (fileSizeInUnit.compareTo(BigDecimal.valueOf(10.0)) >= 0) {
        	// with fileSizeInUnit with 2 digits (10, 11, ..., 99.999) display the first decimal place
            val = fileSizeInUnit.setScale(1, ROUNDING_MODE).toString();
        } else {
        	// with fileSizeInUnit with 1 digit (0, 1, ..., 9.9999) display the first two decimal places
            val = fileSizeInUnit.setScale(2, ROUNDING_MODE).toString();
        }

        // trim zeros at the end
        if (val.endsWith(".00")) {
            val = val.substring(0, val.length() - 3);
        } else if (val.endsWith(".0")) {
            val = val.substring(0, val.length() - 2);
        }

        return String.format("%s %s", val, unit);
    }


    /**
     * Formats a file's size into a human readable format
     *
     * @param fileSize the file's size as long
     * @return the size as human readable string
     */
    public static String byteCountToDisplaySize(final long fileSize) {
        return byteCountToDisplaySize(BigInteger.valueOf(fileSize));
    }

}