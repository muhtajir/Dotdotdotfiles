import imp
import os
WORKING_DIR = os.path.dirname(os.path.realpath(__file__))
imp.load_source('colors', '{}/colors'.format(WORKING_DIR))
from colors import * # i think this is a case where it's okay to do this
