function py.test
    set -lx PYTHONPATH (pwd)
    pytest
end
