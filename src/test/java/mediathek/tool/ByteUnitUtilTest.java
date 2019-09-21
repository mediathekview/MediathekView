package mediathek.tool;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigInteger;

import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Test-Suite for FileUtil class
 */
@ExtendWith(MockitoExtension.class)

public class ByteUnitUtilTest {
	
//	public static void main(String[] args) {
//		// execLoop();
//		new ByteUnitUtilTest().testByteCountToDisplaySizeLong();
//	}

	@SuppressWarnings("unused")
	private static void execLoop() {
		// force boundary cases between byte unit buckets.
		for (int order = 0; order < 10; order++)
		{
			int bits = order*10;
			BigInteger val = BigInteger.ONE.shiftLeft(bits);
			BigInteger pred = val.subtract(BigInteger.ONE);

			BigInteger val1000 = val.multiply(BigInteger.valueOf(1000)); 
			BigInteger val999  = val1000.subtract(BigInteger.ONE);
			
			execOne(pred);
			execOne(val);
			execOne(val999);
			execOne(val1000);
			System.out.println("---");
		}
	}

	private static void execOne(BigInteger val) 
	{
		System.out.print("bitLength = " + val.bitLength());
		System.out.print(" value = " + val.toString());

		//		System.out.print(" bits = " + val.toString(2));
		System.out.print(" exec: " + ByteUnitUtil.byteCountToDisplaySize(val));
		System.out.println();
	}

	public void testByteCountToDisplaySizeLong() {
		final BigInteger twoPow20 = BigInteger.valueOf(1<<20);
		final BigInteger twoPow30 = BigInteger.valueOf(1<<30);
		
		// BYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(0)).isEqualTo("0 bytes");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1)).isEqualTo("1 bytes");
		
		assertThat(ByteUnitUtil.byteCountToDisplaySize(999)).isEqualTo("999 bytes"); 
		// KILOBYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1000)).isEqualTo("0.97 kB"); // ! ROUND DOWN !
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1023)).isEqualTo("0.99 kB"); // ! ROUND DOWN !
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1024)).isEqualTo("1 kB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(10239)).isEqualTo("9.99 kB"); // ! ROUND DOWN !
		assertThat(ByteUnitUtil.byteCountToDisplaySize(10240)).isEqualTo("10 kB");

		assertThat(ByteUnitUtil.byteCountToDisplaySize(1023999)).isEqualTo("999 kB"); 
		// MEGABYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1024000)).isEqualTo("0.97 MB"); // ! ROUND DOWN !
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1048575)).isEqualTo("0.99 MB"); // ! ROUND DOWN !
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1048576)).isEqualTo("1 MB");
		
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1048575999)).isEqualTo("999 MB"); 
		// GIGABYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1048576000)).isEqualTo("0.97 GB"); // ! ROUND DOWN !
		assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30.subtract(BigInteger.ONE))).isEqualTo("0.99 GB"); 
		assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30)).isEqualTo("1 GB"); 
		// TERABYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow20.multiply(twoPow20))).isEqualTo("1 TB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1374389534720L)).isEqualTo("1.25 TB");
		
		// PETABYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30.multiply(twoPow20))).isEqualTo("1 PB");
		
		// EXABYTE Range
		assertThat(ByteUnitUtil.byteCountToDisplaySize(twoPow30.multiply(twoPow30))).isEqualTo("1 EB");

		// please note: the following test boundaries will only work for  
		// 	ROUNDING_MODE = RoundingMode.HALF_UP in class ByteUnitUtil
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(Character.MAX_VALUE)).isEqualTo("64 kB");
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(1023)).isEqualTo("1023 bytes");
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(1030)).isEqualTo("1.01 kB");
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(1224)).isEqualTo("1.20 kB");
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(10239)).isEqualTo("10 kB");
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(24832184)).isEqualTo("23.7 MB");
		// assertThat(ByteUnitUtil.byteCountToDisplaySize(38174914740L)).isEqualTo("35.6 GB");
	}

}
