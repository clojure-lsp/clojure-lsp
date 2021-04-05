#!/usr/bin/env bash
set -e

# Install the latest version of an executable file from a Github release
# Keep a local copy of old versions
# Requires curl

# Customize
#
REPO=clojure-lsp/clojure-lsp
FILE=clojure-lsp

if [[ "$OSTYPE" == "darwin"* ]]; then
    ZIP_FILE=clojure-lsp-native-macos-amd64.zip
else
    ZIP_FILE=clojure-lsp-native-linux-amd64.zip
fi

if [ "$(id -u)" != "0" ]; then
    # Running as non-root user
    DST_DIR=${HOME}/bin
else
    # Running as root (sudo)
    DST_DIR=/usr/bin
fi
#----------

if [ ! -d "${DST_DIR}" ]; then
    echo "${DST_DIR} does not exist or is not a directory. Create it manually to proceed with the install" 1>&2
    echo "Exiting" 1>&2
    exit 1
fi

LATEST_URL="https://github.com/${REPO}/releases/latest"
DOWNLOAD_URL="${LATEST_URL}/download/${ZIP_FILE}"
LOCAL_FILE="./$FILE"
DST_FILE="${DST_DIR}/${FILE}"
CURL_CMD="curl -Ls"

# Get version of an executable
get_exec_version() {
    $1 --version | head -n 1 | awk '{ print $NF }'
}

# Get the latest Github release tag
get_latest_version() {
    latestversion=`${CURL_CMD} -o /dev/null -w %{url_effective} ${LATEST_URL}`
    echo "${latestversion##*/}"
}

echo "Downloading latest ${ZIP_FILE}..."
${CURL_CMD} -o ${ZIP_FILE} ${DOWNLOAD_URL}
if [ ! -s "${ZIP_FILE}" ]; then
    echo "Failed to download from ${DOWNLOAD_URL}"
    exit 1
fi
unzip -o -q ${ZIP_FILE}
chmod 755 ${LOCAL_FILE}
rm $ZIP_FILE
NEW_VERSION=$(get_exec_version ${LOCAL_FILE})

# Get the current (old) version if installed
if [ -f "${DST_FILE}" ]; then
    OLD_VERSION=$(get_exec_version ${DST_FILE})
    echo "Current version: ${OLD_VERSION}"
fi

# No need to do anything if the current installed version is the latest
if [ "${OLD_VERSION}" = "${NEW_VERSION}" ]; then
    echo "Latest ${FILE} is already installed"
    rm ${LOCAL_FILE}
    exit 0
fi

# Backup old version if needed
if [ -n "${OLD_VERSION}" ]; then
    backup="${LOCAL_FILE}-${OLD_VERSION}"
    cp ${DST_FILE} ${backup}
    echo "Old version backed up to ${backup}"
fi

# Install new version
mv -f ${LOCAL_FILE} ${DST_DIR}
echo "Installed new version: ${NEW_VERSION} ($(get_latest_version))"
ls -l ${DST_FILE}
