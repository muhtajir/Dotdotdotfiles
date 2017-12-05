#!/bin/env python
import random

chords = ['Cm', 'C7', 'Dm', 'D7', 'E7', 'F', 'Fm', 'F7', 'G', 'A', 'A7', 'B',
          'B7', 'Fmaj7', 'Dsus2', 'Dsus4', 'Asus2', 'Asus4']

random.shuffle(chords)

print(', '.join(chords))
