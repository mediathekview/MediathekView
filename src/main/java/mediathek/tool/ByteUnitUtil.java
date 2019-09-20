package mediathek.tool;

import static org.apache.commons.io.FileUtils.ONE_YB;
import static org.apache.commons.io.FileUtils.ONE_ZB;
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
 *
 * --
 * internal comment to Sammy Trojette's work 
 * Well, thanks for the idea and for the source code!
 * 
 * Here, Sammy Trojette's idea is adapted but 
 * comparing 
 *      results of imprecise division arithmetics (BigDecimal with a rather narrow scale)
 *    to 
 *      precise byte unit file sizes 
 * I'd rather avoid due to numerical considerations. 
 * So, here division result is not used to determine the byte unit. 
 * Comparisons on BigInteger (a precise operation) are used instead.
 *
 */
public class ByteUnitUtil {

    private static final int DIVISION_SCALE = 5;
	// please note:
	// due to used rounding mode "half up", a file length of 
	// Integer.MAX_VALUE bytes (= 2^31 - 1) will be mapped to 2.00 GB, not to 1.99 GB
	// on other intention, consider using a different rounding mode, e.g.
	//    private static final RoundingMode ROUNDING_MODE = RoundingMode.DOWN;
	private static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_UP;
    
    enum ByteUnit 
    {
    	// please note:
    	// this enumeration deliberately makes a systematic error, confusing 
    	// decimal prefixes kilo, mega, giga, ... (used here for the unit prefix text)
    	// and binary prefixes kibi, mebi, gibi, ... (used here for the number of bytes)

    	// see   https://de.wikipedia.org/wiki/Byte#Vergleich ...
    	
    	// but- how many people who are not computer scientists / it-specialists 
    	// do you know that have ever even heard of Mebibytes and Gibibytes ?

    	// let's face it: for most people MiB is a Hollywood movie, 
    	//   and for the rest of them ... please wait for the red light to flash. BRB. Just adjusting my sunglasses...
    	
    	YOTTABYTE("YB", ONE_YB),       // strictly speaking a YiB (2^80 B) not a YB (10^24 B) - 20.9% error!
    	ZETTYBYTE("ZB", ONE_ZB),       // strictly speaking a ZiB (2^70 B) not a ZB (10^21 B) - 18.1% error!
        EXABYTE("EB", ONE_EB_BI),      // strictly speaking a EiB (2^60 B) not a EB (10^18 B) - 15.3% error!
        PETABYTE("PB", ONE_PB_BI),     // strictly speaking a PiB (2^50 B) not a PB (10^15 B) - 12.6% error!
        TERABYTE("TB", ONE_TB_BI),     // strictly speaking a TiB (2^40 B) not a TB (10^12 B) - 10.0% error!
        GIGABYTE("GB", ONE_GB_BI),     // strictly speaking a GiB (2^30 B) not a GB (10^9  B) -  7.4% error!
        MEGABYTE("MB", ONE_MB_BI),     // strictly speaking a MiB (2^20 B) not a MB (10^6  B) -  4.9% error!
        KILOBYTE("kB", ONE_KB_BI),     // strictly speaking a KiB (2^10 B) not a kB (10^3  B) -  2.4% error!
        BYTE("bytes", BigInteger.ONE); // no prefix means no prefix confusion and hence 0% error

        private final String unitName;
        private final BigInteger byteCount;

        ByteUnit(String unitName, BigInteger byteCount) {
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
	 * @param fileSize the file's size as long
	 * @return the size as human readable string
	 */
	public static String byteCountToDisplaySize(final long fileSize) {
	    return byteCountToDisplaySize(BigInteger.valueOf(fileSize));
	}

	/**
     * Formats a file's size into a human readable format
     *
     * @param fileSizeBytes the file's size as BigInteger
     * @return the size as human readable string
     */
    public static String byteCountToDisplaySize(final BigInteger fileSizeBytes) {
    	ByteUnit byteUnit = getMatchingUnitRange(fileSizeBytes);
        BigDecimal fileSizeInUnit = getFileSizeInUnit(fileSizeBytes, byteUnit);

        String numberAsText = getRoundedFileSizeInUnit(fileSizeInUnit);
        numberAsText = prettyPrintRoundedFileSizeInUnit(numberAsText);

        String byteUnitName = byteUnit.unitName();
        return String.format("%s %s", numberAsText, byteUnitName);
    }

	private static ByteUnit getMatchingUnitRange(BigInteger fileSizeBytes) {
	    for (ByteUnit byteUnit : ByteUnit.values()) {
	        BigInteger byteUnitByteCount = byteUnit.byteCount();
	        if (fileSizeBytes.compareTo(byteUnitByteCount) >= 0) 
	        {
	            return byteUnit;
	        }
	    }
	    return ByteUnit.BYTE;
	}

	private static BigDecimal getFileSizeInUnit(final BigInteger fileSizeBytes, ByteUnit byteUnit) {
		BigDecimal fileSizeBytes_bd = new BigDecimal(fileSizeBytes);
	    BigDecimal byteUnitCount_bd = new BigDecimal(byteUnit.byteCount());
		BigDecimal fileSizeInUnit = fileSizeBytes_bd.divide(byteUnitCount_bd, DIVISION_SCALE, ROUNDING_MODE);
		return fileSizeInUnit;
	}

	private static String getRoundedFileSizeInUnit(BigDecimal fileSizeInUnit) {
		// always round so that at least 3 digits are displayed (###, ##.#, #.##)
		
		// partition interval [0,104[ into three sections:
		// [0,10[ , [10,100[, [100, 1024[    (1 predecimal place, 2 predecimal places, 3 or 4 predecimal places)
		
	    if (fileSizeInUnit.compareTo(BigDecimal.valueOf(100.0)) >= 0) {
	    	// fileSizeInUnit has 3 or 4 predecimal places, i.e. it's in interval [100, 1024[ : do not display any decimal places
	        return fileSizeInUnit.setScale(0, ROUNDING_MODE).toString();
	    } else if (fileSizeInUnit.compareTo(BigDecimal.valueOf(10.0)) >= 0) {
	    	// fileSizeInUnit with 2 predecimal places, i.e. it's in interval [10,100[ : display the first decimal place only
	    	return fileSizeInUnit.setScale(1, ROUNDING_MODE).toString();
	    } else {
	    	// fileSizeInUnit with 1 predecimal places, i.e. it's in interval [0,10[ : display the first two decimal places
	    	return fileSizeInUnit.setScale(2, ROUNDING_MODE).toString();
	    }
	}

	private static String prettyPrintRoundedFileSizeInUnit(String val) {
		// trim zeros at the end
        if (val.endsWith(".00")) {
            return val.substring(0, val.length() - 3);
        } else if (val.endsWith(".0")) {
            return val.substring(0, val.length() - 2);
        }
		return val;
	}

}