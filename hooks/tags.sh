# Return the list of tags for the Docker image passed as a parameter. This is
# pure POSIX shell with minimal dependencies. The code is placed in a subshell
# (parethesis for the body of the function) to ensure that variables are not
# leaked out.
function tags()
(
    im=$1
    if [ -z "$(echo "$im" | grep -o '/')" ]; then
        hub="https://registry.hub.docker.com/v2/repositories/library/$im/tags/"
    else
        hub="https://registry.hub.docker.com/v2/repositories/$im/tags/"
    fi

    # Modified from
    # http://www.googlinux.com/list-all-tags-of-docker-image/index.html to support
    # wget/curl and not rely on jq

    # Get number of pages
    if [ -z "$(command -v curl)" ]; then
        first=$(wget -q -O - $hub)
    else
        first=$(curl -sL $hub)
    fi
    count=$(echo $first | sed -E 's/\{\s*"count":\s*([0-9]+).*/\1/')
    pagination=$(echo $first | grep -Eo '"name":\s*"[a-zA-Z0-9_.-]+"' | wc -l)
    pages=$(expr $count / $pagination + 1)

    # Get all tags one page after the other
    tags=
    i=0
    while [ $i -le $pages ] ;
    do
        i=$(expr $i + 1)
        if [ -z "$(command -v curl)" ]; then
            page=$(wget -q -O - "$hub?page=$i")
        else
            page=$(curl -sL "$hub?page=$i")
        fi
        ptags=$(echo $page | grep -Eo '"name":\s*"[a-zA-Z0-9_.-]+"' | sed -E 's/"name":\s*"([a-zA-Z0-9_.-]+)"/\1/')
        tags="${ptags} $tags"
    done

    echo "$tags"
)
