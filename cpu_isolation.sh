#!/usr/bin/env bash

# isolate cpu 15 for test
sudo cset shield --cpu 14-15

# remove kernel threads from isolated cpu when possible
sudo cset shield -kthread on

# Give users control over the user shield

sudo mkdir -p /dev/cpuset
sudo mount -t cpuset none /dev/cpuset
sudo chmod og+rwX -R /dev/cpuset/user

# run with
# cset shield exec exe -- -arg1 -arg2

# tear down the shield
# cset shield --reset
