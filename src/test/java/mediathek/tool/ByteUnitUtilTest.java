package mediathek.tool;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Test-Suite for ByteUnitUtil class
 */
@ExtendWith(MockitoExtension.class)

public class ByteUnitUtilTest {

    @Test
    void byteRangeTest() {
        // BYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(0)).isEqualTo("0 bytes");
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1)).isEqualTo("1 bytes");
        assertThat(ByteUnitUtil.byteCountToDisplaySize(999)).isEqualTo("999 bytes");
    }

    @Test
    void kilobyteRangeTest() {
        // KILOBYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1000)).isEqualTo("0.97 kB"); // ! ROUND DOWN !
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1023)).isEqualTo("0.99 kB"); // ! ROUND DOWN !
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1024)).isEqualTo("1 kB");
        assertThat(ByteUnitUtil.byteCountToDisplaySize(10239)).isEqualTo("9.99 kB"); // ! ROUND DOWN !
        assertThat(ByteUnitUtil.byteCountToDisplaySize(10240)).isEqualTo("10 kB");

        assertThat(ByteUnitUtil.byteCountToDisplaySize(1023999)).isEqualTo("999 kB");
    }

    @Test
    void megaByteRangeTest() {
        // MEGABYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1024000)).isEqualTo("0.97 MB"); // ! ROUND DOWN !
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1048575)).isEqualTo("0.99 MB"); // ! ROUND DOWN !
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1048576)).isEqualTo("1 MB");

        assertThat(ByteUnitUtil.byteCountToDisplaySize(1048575999)).isEqualTo("999 MB");
    }

    @Test
    void gigabyteRangeTest() {
        final BigInteger twoPow30 = BigInteger.valueOf(1 << 30);
        // GIGABYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1048576000)).isEqualTo("0.97 GB"); // ! ROUND DOWN !
        assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30.subtract(BigInteger.ONE))).isEqualTo("0.99 GB");
        assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30)).isEqualTo("1 GB");
    }

    @Test
    void terabyteRangeTest() {
        final BigInteger twoPow20 = BigInteger.valueOf(1 << 20);

        // TERABYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow20.multiply(twoPow20))).isEqualTo("1 TB");
        assertThat(ByteUnitUtil.byteCountToDisplaySize(1374389534720L)).isEqualTo("1.25 TB");
    }

    @Test
    void petabyteRangeTest() {
        final BigInteger twoPow20 = BigInteger.valueOf(1 << 20);
        final BigInteger twoPow30 = BigInteger.valueOf(1 << 30);
        // PETABYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30.multiply(twoPow20))).isEqualTo("1 PB");
    }

    @Test
    void exabyteRangeTest() {
        final BigInteger twoPow30 = BigInteger.valueOf(1 << 30);
        // EXABYTE Range
        assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30.multiply(twoPow30))).isEqualTo("1 EB");
    }
}
