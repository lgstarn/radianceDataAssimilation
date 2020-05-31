from os import listdir
from os.path import isfile, join, splitext, split, basename
from glob import glob
import subprocess
import sys

if (len(sys.argv) < 6):
    print('Usage: ' + sys.argv[0] + ' [orbits wildcard] [platform #] [posting #] [nsplit] [BHalf file]')
    sys.exit(-1)

orbitsWc   =     sys.argv[1]
platform   = int(sys.argv[2])
posting    = int(sys.argv[3])
nsplit     = int(sys.argv[4])
bhalf      =     sys.argv[5]

print('Using orbits wildcard: ' + orbitsWc)
print('Using platform #: ' + str(platform))
print('Using posting #: ' + str(posting))
print('Using nsplit: ' + str(nsplit))
print('Using BHalf file: ' + bhalf)

for orbitFile in glob(orbitsWc):
    print('Now processing ' + orbitFile)
    filename, file_extension = splitext(orbitFile)
    for splitNum in range(1,nsplit+1):
       cmd = ''
       cmd = cmd + 'qsub -v '
       cmd = cmd +  'orbitFile="' + orbitFile  + '"'
       cmd = cmd + ',platform=' + str(platform) 
       cmd = cmd + ',posting='  +str(posting)
       cmd = cmd + ',outputName="' + basename(orbitFile) + '"'
       cmd = cmd + ',splitNum=' +str(splitNum)
       cmd = cmd + ',nsplit='+str(nsplit)
       cmd = cmd + ',bhalf="'+bhalf + '"'

       cmd = cmd + ' runSplitDeconvolve.sh' 

       print 'Now executing %s' % cmd
       subprocess.call(cmd,shell=True)
