import os
import ntpath
import glob
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("inputFiles",help="a wildcard for the model files to read (must be of type [model string])")
parser.add_argument("outputDirectory",help="the location to place the outputs from the observation operator")
parser.add_argument("fileSuffix",help="the suffix to add to the input file when creating the output file")
parser.add_argument("modelString",help="the code for the state for runRtmForward.exe")
parser.add_argument("platformNum",help="the platform number for the state for runRtmForward.exe")
parser.add_argument("obsOpString",help="the code for the observation operator for runRtmForward.exe")
args = parser.parse_args()

inputFiles = args.inputFiles
outputDirectory = args.outputDirectory
fileSuffix = args.fileSuffix
modelString = args.modelString
platformNum = args.platformNum
obsOpString = args.obsOpString

inputFileGlob = glob.glob(os.path.expanduser(inputFiles))

for inputFile in inputFileGlob:
    outputFile = outputDirectory + ntpath.basename(inputFile) + fileSuffix
    cmd = '../bin/runRtmForward.exe ' + inputFile + ' ' + outputFile + ' ' + modelString + ' ' + platformNum + ' ' + obsOpString
    print(cmd)
    os.system(cmd)
