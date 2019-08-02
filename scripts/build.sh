#!/usr/bin/env bash
mkdir -p build
mkdir -p build/packages
mkdir -p build/resources
mkdir -p build/resources/js
mkdir -p build/resources/css

pug src/app/*.pug -o build/resources

lessc --clean-css src/app/resources/less/mpm.less build/resources/css/mpm.css
lessc --clean-css src/app/resources/less/material-extend.less build/resources/css/material-extend.css

tsc --project .
babel --no-babelrc build -d build --ignore build/packages/ --presets=babili

babel src/molecule.js -o build/molecule.js
babel src/ports.js -o build/ports.js
babel src/menu.js -o build/menu.js
babel src/app/resources/js -d build/resources/js

cp src/app/services/https/package.json build/services/https
cp src/app/services/webgl/package.json build/services/webgl
cp src/app/components/mpm/package.json build/components/mpm
cp src/app/components/helpers/package.json build/components/helpers

ln -sf ~/Downloads/AniList build/packages

printf "{
  \"version\": \"1.0.0\",
  \"active\": { },
  \"settings\": {
    \"restrict\": false
  },
  \"library\": [
    {
      \"score\": \"N/A\",
      \"author\": \"kaketa\",
      \"release\": \"1.0.0\",
      \"name\": \"AniList\",
      \"repository\": \"kaketa/AniList\"
    }
  ]
}" > build/data.json
