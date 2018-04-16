# Automatically Building at the Hub

These hook scripts are used by the Docker hub to [automatically] build a
version of the solution for all existing Alpine versions of InfluxDB.

  [automatically]: https://docs.docker.com/docker-cloud/builds/advanced/

The implementation asks the Docker hub itself for all [tags] available for the
official `influxdb` image, selects those that are relevant and uses these tags
as build [arguments] when building and pushing the backup solution image. Out
of the tag cloud, release candidates `rc` and `meta` and `data` only are taken
away from the `alpine` images in order to minimise the number of builds at the
hub.

  [tags]: https://hub.docker.com/r/library/influxdb/tags/
  [arguments]: https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact

Note that the current implementation is dependent on the hub itself, and it
would perhaps be a better idea to actually use the registry API instead. The
implementation takes inspiration from this stackoverflow [scraping]
implemenation and is almost a direct copy of my generic [gist].

  [scraping]: https://stackoverflow.com/a/43262086
  [gist]: https://gist.github.com/efrecon/4d1334dcb98177bc23af1edd616b4281