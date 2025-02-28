package mediathek.javafx.bookmark;

import javafx.concurrent.Task;
import mediathek.tool.http.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class SearchExpirationDateTask extends Task<String> {
    private static final int EXCERPT_LEN = 1000;
    private static final Pattern[] DATE_PATTERNS = {null, null};
    private static final String[] DATE_PATTERN_STRINGS = {"verfügbar.+?bis.+?([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})", "verfügbar.+?bis.+?([0-9]{2}/[0-9]{2}/[0-9]{4})"};
    private final boolean hasWebURL;
    private final String url;

    public SearchExpirationDateTask(boolean hasWebURL, String url) {
        this.hasWebURL = hasWebURL;
        this.url = url;
    }

    @Override
    protected String call() throws Exception {
        return searchExpiryDate();
    }

    private String searchExpiryDate() {
        String result = null;
        if (hasWebURL) {
            final Request request = new Request.Builder().url(url).get().build();
            try (Response response = MVHttpClient.getInstance().getHttpClient().newCall(request).execute();
                 ResponseBody body = response.body()) {
                if (response.isSuccessful() && body != null) {
                    try (var is = body.byteStream();
                         var isr = new InputStreamReader(is, StandardCharsets.UTF_8);
                         BufferedReader in = new BufferedReader(isr)) {
                        StringBuilder a = new StringBuilder();
                        String str;
                        boolean save = false;
                        // 1.) get EXCERPT_LEN characters beginning with the search term
                        while ((str = in.readLine()) != null) {
                            if (!save) {
                                int idx = str.toLowerCase().indexOf("verfügbar ");
                                if (idx > -1) {
                                    String sdate = str.substring(idx, str.length() - 1); // < idx+EXCERPT_LEN ? str.length() : idx+EXCERPT_LEN);
                                    a.append(sdate);
                                    save = true;
                                }
                            } else {
                                if (a.length() < EXCERPT_LEN) {
                                    a.append(str.toLowerCase());
                                } else {
                                    break;
                                }
                            }
                        }

                        if (!a.isEmpty()) {
                            // 2.) use regex to extract date
                            for (int k = 0; k < DATE_PATTERNS.length; k++) {
                                if (DATE_PATTERNS[k] == null) {   // compile pattern only once!
                                    DATE_PATTERNS[k] = Pattern.compile(DATE_PATTERN_STRINGS[k], Pattern.CASE_INSENSITIVE);
                                }
                                Matcher matcher = DATE_PATTERNS[k].matcher(a);
                                if (matcher.find()) {
                                    result = matcher.group(1).replaceAll("/", "\\.");
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
