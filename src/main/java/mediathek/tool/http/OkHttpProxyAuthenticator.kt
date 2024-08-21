package mediathek.tool.http

import mediathek.tool.ApplicationConfiguration
import okhttp3.Authenticator
import okhttp3.Credentials.basic
import okhttp3.Response
import okhttp3.Route
import org.apache.logging.log4j.LogManager

internal class OkHttpProxyAuthenticator {
    private val logger = LogManager.getLogger()
    var proxyAuthenticator: Authenticator? = null
        private set

    init {
        setupProxyAuthenticator()
    }

    private fun setupProxyAuthenticator() {
        var prxUser = System.getProperty("http.proxyUser")
        var prxPassword = System.getProperty("http.proxyPassword")
        proxyAuthenticator = null
        if (prxUser != null && prxPassword != null && prxUser.isNotEmpty() && prxPassword.isNotEmpty()) {
            //create proxy auth from environment vars
            proxyAuthenticator = createAuthenticator(prxUser, prxPassword)
            logger.info("Proxy Authentication from environment vars: ({})", prxUser)
        } else {
            //try to create proxy auth from settings
            val config = ApplicationConfiguration.getConfiguration()
            prxUser = config.getString(ApplicationConfiguration.HttpProxy.USER, "")
            prxPassword = config.getString(ApplicationConfiguration.HttpProxy.PASSWORD, "")
            if (prxUser.isNotEmpty() && prxPassword.isNotEmpty()) {
                proxyAuthenticator = createAuthenticator(prxUser, prxPassword)
                logger.info("Proxy Authentication from application settings: ({})", prxUser)
            } else
                proxyAuthenticator = null
        }
    }

    private fun createAuthenticator(prxUser: String, prxPassword: String): Authenticator {
        return Authenticator { _: Route?, response: Response ->
            if (response.request.header(HTTP_PROXY_AUTHORIZATION) != null) {
                return@Authenticator null // Give up, we've already attempted to authenticate.
            }
            val credential = basic(prxUser, prxPassword)
            response.request.newBuilder()
                .header(HTTP_PROXY_AUTHORIZATION, credential)
                .build()
        }
    }

    companion object {
        private const val HTTP_PROXY_AUTHORIZATION = "Proxy-Authorization"
    }
}
