# MediathekView

This fork adds the standalone Java headless client under `headless/`. Read
`../CLAUDE.md` and `/home/janosch/DEV/CLAUDE.md` for workspace-wide rules.

## Live headless instance

- Host: `minipc-china-01` (Ubuntu; LAN `10.9.0.100`, OS hostname may be
  `19minipc`), running in the China/Dev site timezone (`Asia/Shanghai`).
- Remote access: use the `meshcentral` skill and the `minipc-china-01` node in
  MeshCentral group `Dev`. Direct SSH to `admin@10.9.0.100` works only when the
  Dev LAN is reachable; do not assume it is available remotely.
- JAR: `/opt/mediathekview-headless/mediathekview-headless.jar`
- configuration: `/etc/mediathekview-headless/subscriptions.json`
- SQLite state/history: `/var/lib/mediathekview-headless/history.db`
- media library: `/mnt/nas-china-01/Kinder/Video/Mediathek`
- service account: `admin`

The API is `mediathekview-headless-api.service`. It listens without
authentication on loopback only at `http://127.0.0.1:7070`; never expose it
directly to an untrusted network. It requires the
`/mnt/nas-china-01/Kinder` NAS mount and has write access only to its state and
media-library paths.

`mediathekview-headless-sync.timer` queues `POST /api/sync` hourly with up to
five minutes of randomized delay. The API owns a single download worker, so
timer and manual work remain serialized. The checked-in unit sources are in
`headless/systemd/`; API usage and deployment assumptions are documented in
`headless/README.md`.

## Read-only status checks

Run these on `minipc-china-01` through MeshCentral or an existing SSH session:

```bash
systemctl status mediathekview-headless-api.service \
  mediathekview-headless-sync.timer --no-pager -l
systemctl list-timers mediathekview-headless-sync.timer --no-pager
curl --fail --silent --show-error http://127.0.0.1:7070/health
curl --fail --silent --show-error http://127.0.0.1:7070/api/downloads
journalctl -u mediathekview-headless-api.service \
  -u mediathekview-headless-sync.service --since "24 hours ago" --no-pager
df -h /var/lib/mediathekview-headless /mnt/nas-china-01/Kinder
```

The sync oneshot exiting successfully means its request was queued, not that
all downloads finished. Use `/api/downloads` to confirm final download states.
