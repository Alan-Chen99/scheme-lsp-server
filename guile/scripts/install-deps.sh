#!/bin/sh

set -e

target_dir=.
curl_path=`which curl`

if ! [ -x $curl_path ]; then
    echo "curl is missing. Aborting installation."
    exit 1
fi

if [ $# -gt 0 ]; then
    case "$1" in
        -p|--prefix) if [ $# -gt 1 ] && [ "$2" != -* ]; then
                         target_dir=$2;
                     fi ;;
        *) echo "usage: $0 [--prefix=PREFIX]"; exit 1 ;;
    esac
fi;

site_dir="${target_dir}/share/guile/site/3.0"
lib_dir="${target_dir}/lib/guile/3.0/site-ccache"


## create base directories
download_dir=`mktemp -d`
mkdir -p $site_dir
mkdir -p $lib_dir

echo "temporary directory for building created: "
echo $download_dir


srfi_url="https://codeberg.org/rgherdt/srfi/archive/main.tar.gz"
irregex_url="http://synthcode.com/scheme/irregex/irregex-0.9.10.tar.gz"
json_rpc_url="https://codeberg.org/rgherdt/scheme-json-rpc/archive/master.tar.gz"

## download tarballs
echo "download_dir: $download_dir"
echo "site_dir: $site_dir"
echo "downloading srfi tarball."
curl $srfi_url -o $download_dir/srfi.tar.gz

echo "downloading irregex tarball."
curl $irregex_url -o $download_dir/irregex.tar.gz

echo "downloading json-rpc tarball."
curl $json_rpc_url -o $download_dir/json-rpc.tar.gz

cd $download_dir
tar xzvf srfi.tar.gz
cd $download_dir/srfi/

export GUILE_LOAD_PATH=.:$site_dir:...:$GUILE_LOAD_PATH
export GUILE_LOAD_COMPILED_PATH=.:$lib_dir:...:$GUILE_LOAD_COMPILED_PATH


## install srfi-145
mkdir -p ${site_dir}/srfi
cp srfi/srfi-145.scm ${site_dir}/srfi

## install irregex
cd ${download_dir}

tar xzvf irregex.tar.gz
mkdir -p ${site_dir}/rx/source
mkdir -p ${lib_dir}/rx/source

cd irregex-0.9.10
cp irregex-guile.scm ${site_dir}/rx/irregex.scm
cp irregex.scm ${site_dir}/rx/source/irregex.scm
cp irregex-utils.scm ${site_dir}/rx/source/irregex-utils.scm
env guild compile --r7rs ${site_dir}/rx/irregex.scm -o ${lib_dir}/rx/irregex.go
env guild compile --r7rs ${site_dir}/rx/source/irregex.scm -o ${lib_dir}/rx/source/irregex.go

## install srfi-180
cd $download_dir/srfi/
mkdir -p ${lib_dir}/srfi
cp srfi/srfi-180.scm ${site_dir}/srfi
cp -R srfi/srfi-180/ ${site_dir}/srfi
cp -R srfi/180/ ${site_dir}/srfi
env guild compile -x "sld" --r7rs ${site_dir}/srfi/srfi-180/helpers.sld -o ${lib_dir}/srfi/srfi-180/helpers.go
env guild compile --r7rs ${site_dir}/srfi/srfi-180.scm -o ${lib_dir}/srfi/srfi-180.go


## install json-rpc
cd $download_dir
tar xzvf json-rpc.tar.gz

cd scheme-json-rpc/guile
echo $GUILE_LOAD_PATH
echo $GUILE_LOAD_COMPILED_PATH
echo `pwd`
./configure --prefix=${target_dir}
make
make install

echo "cleaning up"
rm -R $download_dir
echo "Installation of guile-lsp-server dependencies successfull."
