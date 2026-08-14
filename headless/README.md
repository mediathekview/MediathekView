# MediathekView Headless

`mediathekview-headless` provides search, downloads, subscriptions, download
history, and a local HTTP API without starting Swing, X11, Wayland, or a
virtual display. It is a standalone Java 17 application and does not change
the existing MediathekView desktop entry point.

Searches and exact entry lookups use the current MediathekViewWeb API. Media
is downloaded from the broadcaster URL returned for the selected entry.
Progressive files use Java's HTTP client; HLS playlists use `ffmpeg`.

## Build

Requirements:

- JDK 17 or newer
- `ffmpeg` at runtime when an entry only provides HLS

From the repository root:

```bash
JAVA_HOME=/path/to/jdk ./mvnw -f headless/pom.xml clean verify
java -jar headless/target/mediathekview-headless.jar --help
```

The fat JAR is `headless/target/mediathekview-headless.jar`.

## CLI

Global options such as `--state` and `--base-url` go before the subcommand.

Search and return human-readable rows:

```bash
java -jar headless/target/mediathekview-headless.jar \
  search "Die Sendung mit der Maus" --channel WDR --limit 5
```

Add `--json` for scripting. Each result has a stable `id`; use it for an
exact lookup or download:

```bash
java -jar headless/target/mediathekview-headless.jar \
  show --id 'ENTRY_ID' --json

java -jar headless/target/mediathekview-headless.jar \
  download --id 'ENTRY_ID' \
  --output /mnt/media/MediathekView-Downloads \
  --subdirectory Maus --quality HD
```

The output root must already exist and be writable. Downloads are written to
a partial file in the destination directory and renamed only after success.
The SQLite history prevents downloading the same entry ID twice. `--force`
overrides history for a one-off download.

Other commands:

```bash
java -jar headless/target/mediathekview-headless.jar \
  --state /var/lib/mediathekview-headless/history.db history --json

java -jar headless/target/mediathekview-headless.jar \
  --state /var/lib/mediathekview-headless/history.db \
  sync --config /etc/mediathekview-headless/subscriptions.json
```

## Subscriptions

Copy [`subscriptions.example.json`](subscriptions.example.json), set an
existing `outputDirectory`, and add one or more rules. Query clauses inside a
subscription are combined with AND. A clause can search `channel`, `topic`,
`title`, and/or `description`.

`maxResults` limits the newest matches considered in one run and defaults to
10. Matching entries are downloaded oldest-first within that window. The
history database makes later runs idempotent.

Run the file once with `sync` before enabling a timer. An empty history means
the first run downloads all matching entries in the configured result window.

## HTTP API

Start the server on loopback:

```bash
java -jar headless/target/mediathekview-headless.jar \
  --state /var/lib/mediathekview-headless/history.db \
  serve --bind 127.0.0.1 --port 7070 \
  --config /etc/mediathekview-headless/subscriptions.json
```

Endpoints:

- `GET /health`
- `POST /api/search` — a MediathekViewWeb search request
- `POST /api/entries` — JSON array of exact entry IDs
- `GET /api/downloads` — recent local download history
- `POST /api/downloads` — queue `{id, subdirectory, quality, subtitles}`
- `POST /api/sync` — queue one subscription run

Examples:

```bash
curl --fail --silent --show-error \
  -H 'Content-Type: application/json' \
  --data '{"queries":[{"fields":["title","topic"],"query":"Maus"}],"size":5,"future":false}' \
  http://127.0.0.1:7070/api/search

curl --fail --silent --show-error \
  -H 'Content-Type: application/json' \
  --data '{"id":"ENTRY_ID","subdirectory":"Maus","quality":"HD","subtitles":true}' \
  http://127.0.0.1:7070/api/downloads
```

The API has no authentication and therefore binds to `127.0.0.1` by default.
Use SSH port forwarding or place an authenticated reverse proxy in front of
it; do not bind it to an untrusted network as-is. The complete contract is in
[`openapi.yaml`](openapi.yaml).

## systemd

Example units are in [`systemd/`](systemd/). They assume:

- JAR: `/opt/mediathekview-headless/mediathekview-headless.jar`
- config: `/etc/mediathekview-headless/subscriptions.json`
- state: `/var/lib/mediathekview-headless/history.db`
- service user: `admin`
- NAS mount: `/mnt/nas-china-01/Kinder`

The API service requires the NAS mount and is sandboxed with write access only
to its state directory and the dedicated download directory. The timer calls
the loopback API hourly, so subscription work stays serialized by the API's
single download worker.
