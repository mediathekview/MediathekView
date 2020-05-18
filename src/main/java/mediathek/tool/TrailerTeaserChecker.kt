package mediathek.tool;

/**
 * This class check if titel contains specific keywords.
 */
public class TrailerTeaserChecker {
    public boolean check(String titel) {
        return containsTrailer(titel) || containsTeaser(titel) || containsVorschau(titel);
    }

    private boolean containsTrailer(String titel) {
        return titel.contains("Trailer") || titel.contains("trailer");
    }

    private boolean containsTeaser(String titel) {
        return titel.contains("Teaser") || titel.contains("teaser");
    }

    private boolean containsVorschau(String titel) {
        return titel.contains("Vorschau") || titel.contains("vorschau");
    }
}
