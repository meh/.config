#! /bin/zsh

screenruster daemon &
dux adaptive -R 0 &

exec fluxbox
