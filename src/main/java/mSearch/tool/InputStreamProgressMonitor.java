package mSearch.tool;

/**
 * User: Christian F.
 * Date: 15.06.16
 * Time: 14:05
 */
public interface InputStreamProgressMonitor {
    void progress(long bytesRead, long size);
}
