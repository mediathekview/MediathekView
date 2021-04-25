/*
 *    MVSingleInstance
 *    Copyright (C) 2013 CrystalPalace
 *    crystalpalace1977@googlemail.com
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

import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;

/**
 * Prevents startup of multiple instances
 */
final public class SingleInstance implements AutoCloseable {

    private FileChannel channel;
    private FileLock lock;
    private final File file = new File(SystemUtils.JAVA_IO_TMPDIR, "MediathekView.lock");
    private final RandomAccessFile raf;

    public SingleInstance() throws FileNotFoundException {
        raf = new RandomAccessFile(file, "rw");
    }

    public boolean isAppAlreadyActive() {
        try {
            //store lock in temp directory, will not survive reboot
            channel = raf.getChannel();

            lock = channel.tryLock();
            if (lock == null) {
                //we could not acquire the lock because another app already holds it...we are already active
                closeLock();
                return true;
            }

            //delete the lockfile when VM gets shut down
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                closeLock();
                try {
                    Files.deleteIfExists(file.toPath());
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }));
            return false;
        } catch (Exception e) {
            //if there is any sort of error, pretend we are already running...
            closeLock();
            return true;
        }
    }

    private void closeLock() {
        try {
            lock.release();
            channel.close();
        } catch (Exception ignored) {
        }
    }

    @Override
    public void close() throws Exception {
        raf.close();
    }
}