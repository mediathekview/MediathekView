/*    
 *    MediathekView
 *    Copyright (C) 2013   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 * 
 *    org.apache.hadoop.tools.util.ThrottledInputStream
 *    unter http://www.apache.org/licenses/LICENSE-2.0
 *    diente als Vorlage
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import java.io.IOException;
import java.io.InputStream;

import mediathek.controller.starter.MVBandwidthTokenBucket;

public class MVInputStream extends InputStream {

    private final InputStream iStream;
    private long startZeit = System.currentTimeMillis();
    private long bytesGelesen;
    private MVBandwidthTokenBucket bucket = null;

    public MVInputStream(InputStream in) {
        iStream = in;
        bucket = MVBandwidthTokenBucket.getInstance();
        bucket.ensureBucketThreadIsRunning();
    }

    @Override
    public void close() throws IOException
    {
        iStream.close();
        super.close();
    }

    @Override
    public int read() throws IOException {
        bucket.takeBlocking();
        final int einByte = iStream.read();
        if (einByte != -1) {
            bytesGelesen++;
        }
        return einByte;
    }

    @Override
    public int read(byte[] b) throws IOException {
        bucket.takeBlocking();
        final int anzByte = iStream.read(b);
        if (anzByte != -1) {
            bytesGelesen += anzByte;
        }
        return anzByte;
    }

    public long getBandbreite() {
        final long dauer = (System.currentTimeMillis() - startZeit) / 1000;
        if (dauer == 0) {
            return bytesGelesen;
        } else {
            return bytesGelesen / dauer;
        }
    }

    @Override
    public String toString() {
        return "Download: "
                + "gelesen: " + (bytesGelesen > 0 ? bytesGelesen / 1024 : 0) + " KiB, "
                + "Bandbreite: " + (getBandbreite() > 0 ? getBandbreite() / 1024 : 0) + " KiB/s ";
    }
}
