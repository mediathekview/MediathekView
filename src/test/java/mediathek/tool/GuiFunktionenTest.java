package mediathek.tool;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class GuiFunktionenTest {

    @Test
    void getSuffixFromUrl() {
        var testStr = "http://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd";
        var res = GuiFunktionen.getSuffixFromUrl(testStr);

        Assertions.assertEquals(res, "m3u8");
    }

    @Test
    void getFileNameWithoutSuffix() {
        var testStr = "http://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd";
        var res = GuiFunktionen.getFileNameWithoutSuffix(testStr);

        Assertions.assertEquals(res, "http://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av");
    }
}