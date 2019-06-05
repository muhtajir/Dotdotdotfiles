function pyfart
    set venvdir (mktemp --tmpdir -d pyfart-XXX)
    virtualenv "$venvdir"
    source "$venvdir"/bin/activate.fish
end
