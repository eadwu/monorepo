#!/usr/bin/env bash
mkdir -p build
mkdir -p build/resources
mkdir -p build/resources/jsx
mkdir -p build/resources/css

pug src/index.pug -o build
lessc --clean-css src/index.less build/resources/css/index.css
babel src/index.jsx -o build/resources/jsx/index.jsx
