#!/bin/bash
for i in 57110 57111 57112 57113 57114 57115 57116 57117
   do 
        echo $i
        scsynth -w 128 -u $i &
   done
