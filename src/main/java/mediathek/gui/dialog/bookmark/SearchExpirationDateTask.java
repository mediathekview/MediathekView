package mediathek.gui.dialog.bookmark;

import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

import javax.swing.SwingWorker;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class SearchExpirationDateTask extends SwingWorker<String, Void> {

    private static final int EXCERPT_LEN = 1000;
    private static final Pattern[] DATE_PATTERNS = {null, null};
    private static final String[] DATE_PATTERN_STRINGS = {
        "verfügbar.+?bis.+?([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})",
        "verfügbar.+?bis.+?([0-9]{2}/[0-9]{2}/[0-9]{4})"
    };

    private final boolean hasWebURL;
    private final String url;

    public SearchExpirationDateTask(boolean hasWebURL, String url) {
        this.hasWebURL = hasWebURL;
        this.url = url;
    }

    @Override
    protected String doInBackground() throws Exception {
        return searchExpiryDate();
    }

    private String searchExpiryDate() {
        String result = null;
        if (hasWebURL) {
            Request request = new Request.Builder().url(url).get().build();
            try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
                ResponseBody body = response.body()) {
                if (response.isSuccessful() && body != null) {
                    try (BufferedReader in = new BufferedReader(
                        new InputStreamReader(body.byteStream(), StandardCharsets.UTF_8))) {

                        StringBuilder excerpt = new StringBuilder();
                        String line;
                        boolean foundStart = false;

                        while ((line = in.readLine()) != null) {
                            if (!foundStart) {
                                int idx = line.toLowerCase().indexOf("verfügbar ");
                                if (idx > -1) {
                                    excerpt.append(line.substring(idx));
                                    foundStart = true;
                                }
                            } else {
                                if (excerpt.length() < EXCERPT_LEN) {
                                    excerpt.append(line.toLowerCase());
                                } else {
                                    break;
                                }
                            }
                        }

                        if (!excerpt.isEmpty()) {
                            for (int i = 0; i < DATE_PATTERNS.length; i++) {
                                if (DATE_PATTERNS[i] == null) {
                                    DATE_PATTERNS[i] = Pattern.compile(DATE_PATTERN_STRINGS[i], Pattern.CASE_INSENSITIVE);
                                }
                                Matcher matcher = DATE_PATTERNS[i].matcher(excerpt);
                                if (matcher.find()) {
                                    result = matcher.group(1).replaceAll("/", ".");
                                    break;
                                }
                            }
                        }
                    }
                }
            } catch (IOException ignored) {
            }
        }
        return result;
    }
}
