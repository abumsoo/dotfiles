#!/bin/zsh

# i3-msg "workspace 1; append_layout ~/.i3/text-edit.json"
i3-msg workspace 1
(emacsclient -c ~/gitrepos/notProgramming/goals.org &)

sleep 1

# i3-msg "workspace 10; append_layout ~/.i3/browser.json"
(firefox &)
i3-msg workspace 2
sleep 3
i3-msg workspace 1

cd ~/gitrepos/notProgramming && git pull &
