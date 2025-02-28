package mediathek.tool;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class GuiFunktionenTest {

    @Test
    void getFilmListUpdateType() {
        var res = GuiFunktionen.getFilmListUpdateType();
        Assertions.assertSame(FilmListUpdateType.AUTOMATIC, res);
    }

    @Test
    void getSuffixFromUrl() {
        final var testStr = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd";
        final var expected = "m3u8";
        var res = GuiFunktionen.getSuffixFromUrl(testStr);

        Assertions.assertEquals(expected, res);
    }

    @Test
    void getFileNameWithoutExtension_web() {
        final var testStr = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd";
        final var expected = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av";
        var res = GuiFunktionen.getFileNameWithoutExtension(testStr);

        Assertions.assertEquals(expected, res);
    }

    @Test
    void getFileNameWithoutExtension_file() {
        final var testStr = "/Users/derreisende/file1.mp4";
        final var expected = "/Users/derreisende/file1";
        var res = GuiFunktionen.getFileNameWithoutExtension(testStr);

        Assertions.assertEquals(expected, res);
    }
}