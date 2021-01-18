#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd "$(dirname $0)"

top_entity=${2:-contranomy}

rm -rf work ./*.o ./*.cf

mkdir work
mkdir -p waveforms

ghdl -i --workdir=work ./"$1"/*.vhdl
ghdl -m --workdir=work "${top_entity}"
ghdl -r --workdir=work "${top_entity}" --wave=waveforms/"${top_entity}.ghw"
ghdl --remove --workdir=work
rm -d work
