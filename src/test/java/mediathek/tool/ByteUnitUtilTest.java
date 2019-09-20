package mediathek.tool;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Test-Suite for FileUtil class
 */
@ExtendWith(MockitoExtension.class)

public class ByteUnitUtilTest {

	public void testByteCountToDisplaySizeLong() {
		// please note: this test boundaries will only work for  
		// 	ROUNDING_MODE = RoundingMode.HALF_UP in class ByteUnitUtil
		
		assertThat(ByteUnitUtil.byteCountToDisplaySize(Character.MAX_VALUE)).isEqualTo("64 KB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(Integer.MAX_VALUE)).isEqualTo("2 GB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(0)).isEqualTo("0 bytes");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1)).isEqualTo("1 bytes");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1023)).isEqualTo("1023 bytes");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1024)).isEqualTo("1 KB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1030)).isEqualTo("1.01 KB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1224)).isEqualTo("1.20 KB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(10239)).isEqualTo("10 KB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(10240)).isEqualTo("10 KB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(24832184)).isEqualTo("23.7 MB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(38174914740L)).isEqualTo("35.6 GB");
		assertThat(ByteUnitUtil.byteCountToDisplaySize(1374389534720L)).isEqualTo("1.25 TB");
	}

}
