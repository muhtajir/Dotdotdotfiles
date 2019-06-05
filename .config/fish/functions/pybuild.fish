function pybuild
    [ -e setup.py ]; or return 1
    python setup.py sdist bdist_wheel
    pass show -c random/package-signing
    set -l pkg_version (string replace -rf '^\s+version=.([\d.]+).+' '$1' < setup.py) 
    cd dist
    gpg --detach-sign --local-user 7F6D8FF4 -a *$pkg_version*
    pass show -c random/python_package_index\%twine
    twine upload *$pkg_version*
    cd ..
end
