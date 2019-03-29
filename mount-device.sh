#!/bin/bash
now=$(date)
echo "NEW MOUNT AT : $now" >> /home/mark/mount.log
echo "message" | netcat localhost 4242
