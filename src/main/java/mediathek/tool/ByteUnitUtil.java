package mediathek.tool;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import static org.apache.commons.io.FileUtils.*;

/**
 * This code is inspired by
 * custom Implementation of FileUtils.byteCountToDisplaySize to fix rounding bug
 * taken from https://issues.apache.org/jira/browse/IO-373 (2019-09-20)
 * post from Sammy Trojette, 2016-11-01-10-09
 * <p>
 * quote:
 * Since this has just come up as an issue on our current project. I'd like to do a little necro here.
 * With this version, the displayed value is always set at least 3 numericals so: ####, ###, ##.# or #.##.
 * If anyone sees a serious issue, please give me a call.
 * <p>
 * --
 * Well, what Sammy Trojette did, is not actually exactly what e.g. Windows does. // TODO- test other OS.
 * 1) Windows will round down, at least in displaying free space and when displaying file properties.
 * Rounding mode half up is not used here.
 * e.g. 2^30 B is displayed as 1 TB,
 * but (2^30-1) B is displayed as 0.99 TB
 * 2) Windows will not display 4 digits in file properties,
 * i.e. there is no displaying #,### UNIT.
 * Display will reduce to 3 digits and thus display will be 0.## NEXTUNIT instead.
 * <p>
 * For every value less than 1000 YiB (which corresponds to ~ 10^15 discs with 1 TB capacity each),
 * what this tool will give is always of the form ### UNIT, ##.# UNIT or #.## UNIT
 * <p>
 * (if you had a disc with 1 TB capacity sized a cube of length 1cm,
 * that would be a cube of 10^5 cm side length, or 1000m.)
 */
public class ByteUnitUtil {

    private static final int DIVISION_SCALE = 2;                         // we will never need more than 2 decimal digits for display
    private static final RoundingMode ROUNDING_MODE = RoundingMode.DOWN; // rounding down! i.e. (2^30-1) B is to be displayed as 0.99 TB

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

//      testing purposes
//		System.out.print("fileSizeBytes = " + fileSizeBytes.toString());
//		System.out.print(" bitLength = " + fileSizeBytes.bitLength());
//		System.out.print(" byteUnit = " + byteUnit.name());
//		System.out.println();

        String numberAsText = getRoundedFileSizeInUnit(fileSizeInUnit);
        numberAsText = prettyPrintRoundedFileSizeInUnit(numberAsText);

        String byteUnitName = byteUnit.unitName();
        return String.format("%s %s", numberAsText, byteUnitName);
    }

    private static ByteUnit getMatchingUnitRange(BigInteger fileSizeBytes) {
        // compute floor(log2(filSizeBytes)) to get to a raw slot.
        int bitLength = fileSizeBytes.bitLength();
        ByteUnit byteUnitRaw = ByteUnit.fromBitLength(bitLength);

        // tested with MS Windows:
        // OS will use the next higher byte unit prefix if more than 3 predecimal places would be required to represent size
        // so, if the fileSize is 1000 times the minimum size of the byte unit or more,
        // imitate OS behavior and step up to the next byte unit
        BigInteger bytesRawUnit = byteUnitRaw.byteCount();
        BigInteger thousandTimesRawUnit = bytesRawUnit.multiply(BigInteger.valueOf(1000));

        if (fileSizeBytes.compareTo(thousandTimesRawUnit) >= 0) {
            // System.out.print("exceeding 3-digit scale.");
            // System.out.println("Switch from " + byteUnitRaw.name() + " to next byteUnit " + byteUnitRaw.next().name());
            return byteUnitRaw.next();
        }
        return byteUnitRaw;
    }

    private static BigDecimal getFileSizeInUnit(final BigInteger fileSizeBytes, ByteUnit byteUnit) {
        // compute the factor, how many times the byte count of the byte unit do we have (rounded to a few decimal places)
        BigDecimal fileSizeBytes_bd = new BigDecimal(fileSizeBytes);
        BigDecimal byteUnitCount_bd = new BigDecimal(byteUnit.byteCount());
        return fileSizeBytes_bd.divide(byteUnitCount_bd, DIVISION_SCALE, ROUNDING_MODE);
    }

    private static String getRoundedFileSizeInUnit(BigDecimal fileSizeInUnit) {
        // always round so that 3 digits are displayed (###, ##.#, #.##)

        // partition interval [0,1000[ into three sections:
        // [0,10[ , [10,100[, [100, 1000[    (1 predecimal place, 2 predecimal places, 3 or 4 predecimal places)
        // [1000, 1024[ can not occur by design of getMatchingUnitRange

        if (fileSizeInUnit.compareTo(BigDecimal.valueOf(100.0)) >= 0) {
            // fileSizeInUnit with 3 predecimal places, i.e. it's in interval [100, 999[ : do not display any decimal places
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
        // trim trailing zeros
        if (val.endsWith(".00")) {
            return val.substring(0, val.length() - 3);
        } else if (val.endsWith(".0")) {
            return val.substring(0, val.length() - 2);
        }
        // else: for #.#0, the trailing 0 is not truncated
        return val;
    }

    // enumerate Byte units with
    // - the size of binary prefixes (the 1024 thingy) see
    // - the names of the more commonly known decimal prefixes (the 1000 thingy)
    //
    // sizes
    // see https://de.wikipedia.org/wiki/Bin%C3%A4rpr%C3%A4fix
    // and see org.apache.commons.io.FileUtils for actual definitions of the constants used used here
    //
    // names
    // see  https://de.wikipedia.org/wiki/Vors%C3%A4tze_f%C3%BCr_Ma%C3%9Feinheiten
    enum ByteUnit {
        // please note:
        // this enumeration deliberately makes a systematic error, confusing
        // decimal prefixes kilo, mega, giga, ... (used here for the unit prefix text)
        // and binary prefixes kibi, mebi, gibi, ... (used here for the number of bytes)

        // see   https://de.wikipedia.org/wiki/Byte#Vergleich ...

        // but- how many people who are not computer scientists / it-specialists
        // do you know that have ever even heard of Mebibytes and Gibibytes ?

        // let's face it: for most people MiB is a Hollywood movie,
        //   and for the rest of them ... please wait for the red light to flash. BRB. Just adjusting my sunglasses...

        BYTE("bytes", BigInteger.ONE), // no unit prefix means no prefix confusion and hence 0% error
        KILOBYTE("kB", ONE_KB_BI),     // strictly speaking a KiB (2^10 B) not a kB (10^3  B) -  2.4% error! (lower case k in kB! upper case K in KiB))
        MEGABYTE("MB", ONE_MB_BI),     // strictly speaking a MiB (2^20 B) not a MB (10^6  B) -  4.9% error!
        GIGABYTE("GB", ONE_GB_BI),     // strictly speaking a GiB (2^30 B) not a GB (10^9  B) -  7.4% error!
        TERABYTE("TB", ONE_TB_BI),     // strictly speaking a TiB (2^40 B) not a TB (10^12 B) - 10.0% error!
        PETABYTE("PB", ONE_PB_BI),     // strictly speaking a PiB (2^50 B) not a PB (10^15 B) - 12.6% error!
        EXABYTE("EB", ONE_EB_BI),      // strictly speaking a EiB (2^60 B) not a EB (10^18 B) - 15.3% error!

        // well, BigInteger may allow arbitrary precision with the shortcoming of non-hardware support for arithmetics.
        // and hence less performance. Why not compute using long? On the other hand,
        // if this tool is not called millions of times, performance should not be a real issue here.

        // With long (64 bit), we can actually handle up to 8 EiB. In other words 8 million discs with 1TB capacity each.
        // however, this tool is meant as an alternative for org.apache.commons.io.FileUtils.byteCountToDisplaySize
        // which accepts BigINteger input. To resemble that interface, this also uses BigInteger internally.

        // so, if we are talking BigInteger, let's not stop with 64-bit file sizes and according limit to byte units.
        // There is more:
        ZETTABYTE("ZB", ONE_ZB),       // strictly speaking a ZiB (2^70 B) not a ZB (10^21 B) - 18.1% error!
        YOTTABYTE("YB", ONE_YB),       // strictly speaking a YiB (2^80 B) not a YB (10^24 B) - 20.9% error!
        // end of prefixes defined by BIPM. More to come 2022 on next BIPM general convention?
        ;

        private final String unitName;      // decimal unit name
        private final BigInteger byteCount; // binary unit size

        ByteUnit(String unitName, BigInteger byteCount) {
            this.unitName = unitName;
            this.byteCount = byteCount;
        }

        // "logarithmic" getter
        static ByteUnit fromBitLength(int bitLength) {
            // bitLength of a positive integer is something like a rounded down binary logarithm
            // special case: BigInteger.ZERO has bitLength 0, not "negative infinity".

            // the unit prefixes apply to intervals of 10 bits
            // [0 bits and 1-10 bits], [11 bits - 20 bits], [21 bits - 30 bits], [31 bits - 40 bits], ...
            // Bytes                 , KibiBytes          , MebiBytes          , GibiBytes
            int ordinalRaw = Math.max(bitLength - 1, 0) / 10; // BigInteger.ZERO has bitLength 0. map 0 bits to bucket 0 ("BYTE")
            int ordinal = Math.min(ordinalRaw, values().length - 1); // logarithm can be infinite. stop at the largest defined prefix.
            final ByteUnit byteUnit = values()[ordinal];
            // System.out.println("fromBitLength ordinal: " + ordinal + " -> " + byteUnit.name());
            return byteUnit;
        }

        ByteUnit previous() {
            return values()[Math.max(ordinal() - 1, 0)];
        }

        ByteUnit next() {
            return values()[Math.min(ordinal() + 1, values().length - 1)];
        }

        private String unitName() {
            return unitName;
        }

        private BigInteger byteCount() {
            return byteCount;
        }
    }

}