package mediathek.filmlisten;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import mediathek.daten.ListeFilme;

class FilmeImportierenDateiThread extends Thread {

    private final String pfad;
    private final ListeFilme listeFilme;
    private final int days;
    private final IAction onFinishedAction;
    private final IDownloadAction downloadAction;

    public FilmeImportierenDateiThread(String pfad, ListeFilme listeFilme, int days, IDownloadAction downloadAction,
                                       IAction onFinished) {
        // is directory-path of 'pfad' really a directory (relative/fix) ? (-> convert 'pfad' to directory if it is a file...)
        //File file_pfad_dir = new File(getDirnameFromPathname(pfad));
        if(new File(pfad).exists()) {
            this.pfad = pfad;
        } else {
            this.pfad = System.getProperty("user.dir") + File.separator + pfad;
        }
        this.listeFilme = listeFilme;
        this.days = days;
        onFinishedAction = onFinished;
        this.downloadAction = downloadAction;

        setName("FilmeImportierenDateiThread");
    }

    @Override
    public void run() {
        final boolean result = downloadAction.performDownload(pfad, listeFilme, days);
        onFinishedAction.onFinished(result);
    }
    
    public static String getDirnameFromPathname(String pathname)
    {
        Path path = Paths.get(pathname);
        String dirname = path.getRoot().toString()+path.subpath(0, path.getNameCount()-1).toString();
        return(dirname);
    }
}
