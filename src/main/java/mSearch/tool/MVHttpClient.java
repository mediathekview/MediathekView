package mSearch.tool;

import okhttp3.Authenticator;
import okhttp3.Credentials;
import okhttp3.OkHttpClient;
import org.apache.commons.configuration2.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.NoSuchElementException;
import java.util.concurrent.TimeUnit;

public class MVHttpClient {
    private static final MVHttpClient ourInstance = new MVHttpClient();
    private static final String HTTP_PROXY_AUTHORIZATION = "Proxy-Authorization";
    private final Logger logger = LogManager.getLogger(MVHttpClient.class);
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private OkHttpClient httpClient;
    private OkHttpClient copyClient;

    private MVHttpClient() {
        String proxyHost = System.getProperty("http.proxyHost");
        String proxyPort = System.getProperty("http.proxyPort");

        try {
            if (proxyHost != null && proxyPort != null && !proxyHost.isEmpty() && !proxyPort.isEmpty()) {
                //we are configuring the proxy from environment variables...
                final Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, Integer.parseInt(proxyPort)));
                setupProxyClients(proxy);
                logger.info("MVHttpClient: Proxy configured from environment variables: ({})", proxyHost);
            } else {
                //environment variables were not set, use application settings...
                try {
                    proxyHost = config.getString(ApplicationConfiguration.HTTP_PROXY_HOSTNAME);
                    proxyPort = config.getString(ApplicationConfiguration.HTTP_PROXY_PORT);
                    if (!proxyHost.isEmpty() && !proxyPort.isEmpty()) {
                        final Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyHost, Integer.parseInt(proxyPort)));
                        setupProxyClients(proxy);
                        logger.info("MVHttpClient: Proxy configured from application config: ({})", proxyHost);
                    } else {
                        //no proxy setup specified...
                        setupNonProxyClients();
                    }
                } catch (NoSuchElementException e) {
                    setupNonProxyClients();
                }
            }
        } catch (NumberFormatException ex) {
            setupNonProxyClients();
            logger.error("PROXY config failed. Creating non proxy config", ex);
        }
    }

    public static MVHttpClient getInstance() {
        return ourInstance;
    }

    /**
     * Build the client builder with default settings.
     *
     * @return A Builder with default settings.
     */
    private OkHttpClient.Builder getDefaultClientBuilder() {
        return new OkHttpClient.Builder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS);
    }

    private Authenticator createAuthenticator(String prxUser, String prxPassword) {
        return (route, response) -> {
            if (response.request().header(HTTP_PROXY_AUTHORIZATION) != null) {
                return null; // Give up, we've already attempted to authenticate.
            }
            final String credential = Credentials.basic(prxUser, prxPassword);
            return response.request().newBuilder()
                    .header(HTTP_PROXY_AUTHORIZATION, credential)
                    .build();
        };
    }

    private Authenticator setupProxyAuthenticator() {
        String prxUser = System.getProperty("http.proxyUser");
        String prxPassword = System.getProperty("http.proxyPassword");
        Authenticator proxyAuthenticator = null;

        if (prxUser != null && prxPassword != null && !prxUser.isEmpty() && !prxPassword.isEmpty()) {
            //create proxy auth from environment vars
            proxyAuthenticator = createAuthenticator(prxUser, prxPassword);
            logger.info("Proxy Authentication from environment vars: ({})", prxUser);
        } else {
            //try to create proxy auth from settings
            try {
                prxUser = config.getString(ApplicationConfiguration.HTTP_PROXY_USERNAME);
                prxPassword = config.getString(ApplicationConfiguration.HTTP_PROXY_PASSWORD);
                if (!prxUser.isEmpty() && !prxPassword.isEmpty()) {
                    proxyAuthenticator = createAuthenticator(prxUser, prxPassword);
                    logger.info("Proxy Authentication from application settings: ({})", prxUser);
                }
            } catch (NoSuchElementException ignored) {
            }
        }

        return proxyAuthenticator;
    }

    /**
     * Set the proxy parameters on the shared HTTP clients.
     *
     * @param proxy The proxy settings to be used.
     */
    private void setupProxyClients(Proxy proxy) {
        final Authenticator proxyAuthenticator = setupProxyAuthenticator();

        OkHttpClient.Builder tmpBuilder;
        tmpBuilder = getDefaultClientBuilder()
                .proxy(proxy);

        if (proxyAuthenticator != null)
            tmpBuilder.proxyAuthenticator(proxyAuthenticator);
        httpClient = tmpBuilder.build();

        tmpBuilder = getDefaultClientBuilder()
                .connectTimeout(5, TimeUnit.SECONDS)
                .readTimeout(5, TimeUnit.SECONDS)
                .writeTimeout(2, TimeUnit.SECONDS)
                .proxy(proxy);

        if (proxyAuthenticator != null)
            tmpBuilder.proxyAuthenticator(proxyAuthenticator);
        copyClient = tmpBuilder.build();
    }

    /**
     * Setup HTTP client without proxy settings
     */
    private void setupNonProxyClients() {
        httpClient = getDefaultClientBuilder()
                .build();

        copyClient = getDefaultClientBuilder()
                .connectTimeout(5, TimeUnit.SECONDS)
                .readTimeout(5, TimeUnit.SECONDS)
                .writeTimeout(2, TimeUnit.SECONDS)
                .build();

        logger.info("MVHttpClient: Proxy not configured");
    }

    public OkHttpClient getHttpClient() {
        return httpClient;
    }

    public OkHttpClient getReducedTimeOutClient() {
        return copyClient;
    }
}
