package mediathek.tool;

public interface InputStreamProgressMonitor {
    void progress(long bytesRead, long size);
}
