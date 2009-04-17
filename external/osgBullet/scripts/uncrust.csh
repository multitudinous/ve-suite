find . -name "*\.cpp" | xargs -n 1 uncrustify --replace --no-backup -c scripts/uncrustify.cfg
find . -name "*\.h" | xargs -n 1 uncrustify --replace --no-backup -c scripts/uncrustify.cfg