#!/bin/bash

stack build --flag tasktags:release
sudo cp $(stack path --local-install-root)/bin/keepToMd /usr/local/bin
sudo cp $(stack path --local-install-root)/bin/togglCsv /usr/local/bin
sudo cp $(stack path --local-install-root)/bin/togglSubmit /usr/local/bin
