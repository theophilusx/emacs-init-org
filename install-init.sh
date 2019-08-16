#!/bin/bash

today=`date +'%Y%m%d%H%M%S'`

if [[ -f ./tangle-early-init.el ]]; then
    echo "Found new early-init.el.  Will backup to early-init.el-${today}"
    mv ./early-init.el ./early-init.el-${today} && mv ./tangle-early-init.el ./early-init.el
else
    echo "No new early-init.el found"
fi

if [[ -f ./tangle-init.el ]]; then
    echo "Found new init.el. Will backup to init.el.el-${today}"
    mv ./init.el ./init.el-${today} && mv ./tangle-init.el ./init.el
else
    echo "No new init.el found"
fi
