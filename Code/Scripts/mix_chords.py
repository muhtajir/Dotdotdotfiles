import random

chords = ['C', 'Cm', 'C7', 'D', 'Dm', 'D7', 'E', 'Em', 'E7', 'F', 'Fm', 'F7',
          'G', 'Gm', 'G7', 'A', 'Am', 'A7', 'B', 'Bm', 'B7', 'Fmaj7']

random.shuffle(chords)

print(', '.join(chords))
