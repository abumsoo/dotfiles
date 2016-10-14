#!/bin/zsh

i3-msg "workspace 1; append_layout ~/.i3/text-edit.json"
(emacsclient -c ~/gitrepos/notProgramming/goals.org &)

sleep 1

i3-msg "workspace 10; append_layout ~/.i3/browser.json"
(chromium &)
