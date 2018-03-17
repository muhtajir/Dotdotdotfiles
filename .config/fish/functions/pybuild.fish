function pybuild
    [ -e setup.py ]; or return 1
    python setup.py sdist bdist_wheel
    pass show -c random/package-signing
    set -l version (string replace -rf '^\s+version=.([\d.]+).+' '$1' < setup.py) 
    cd dist
    gpg --detach-sign --local-user 7F6D8FF4 -a *$version*
    pass show -c random/python_package_index\%twine
    twine upload *$version*
    cd ..
end
