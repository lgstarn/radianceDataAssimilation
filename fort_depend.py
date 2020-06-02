#!/usr/bin/python
import os
import re
import sys

#Definitions
def run(files=None,verbose=True,overwrite=None,output=None,macros={},build=''):

    l,fil2fil=create_file_objs(files,macros)
    mod2fil=file_objs_to_mod_dict(file_objs=l)
    depends=get_depends(fob=l,m2f=mod2fil,f2f=fil2fil)

    if verbose:
        for i in depends.keys():
            print(""+i+" depends on :")
            for j in depends[i]: print("\t"+j)
            print("")

    if output is None:
        output = "makefile.dep"

    tmp=write_depend(outfile=output,dep=depends,overwrite=overwrite,build=build)

    return depends

def write_depend(outfile="makefile.dep",dep=[],overwrite=False,build=''):
    "Write the dependencies to outfile"
    #Test file doesn't exist
    if os.path.exists(outfile):
        if not(overwrite):
            print("Warning file exists.")
            opt="y" #raw_input("Overwrite? Y... for yes.")
        else:
            opt="y"
        if opt.lower().startswith("y"):
            pass
        else:
            return

    #Open file
    f=open(outfile,'w')
    f.write('# This file is generated automatically. DO NOT EDIT!\n')
    for i in dep.keys():

        if (".exe" in i):
            tmp,fil=os.path.split(i)
            stri="\nbin/"+fil+" := "
            fname = i.replace('src/','build/')
            tmp,fil=os.path.split(fname)
            stri=stri+os.path.join(tmp, fil.split(".")[0]+".o")
            for j in dep[i]:
                tmp,fil=os.path.split(j)
                stri=stri+" \\\n\t"+os.path.join(build, tmp.replace('src/','build/'), fil.split(".")[0]+".o")
            stri=stri+"\n"
            f.write(stri)
        else:
            fname = i.replace('src/','build/')
            tmp,fil=os.path.split(fname)
            stri="\n"+os.path.join(tmp, fil.split(".")[0]+".o") + ":"
            for j in dep[i]:
                if (any(x in j for x in [".f90",".F90",".f03",".F03",".f",".F"])):
                    tmp,fil=os.path.split(j)
                    stri=stri+" \\\n\t"+os.path.join(build, tmp.replace('src/','build/'), fil.split(".")[0]+".o")
                else:
                    stri=stri+" \\\n\t"+j
            stri=stri+"\n"
            f.write(stri)

    f.close()
    return

def get_source(ext=[".f90",".F90",".f03",".F03",".f",".F"]):
    "Return all files ending with any of ext"
    tmp=[os.path.join(dp, f) for dp, dn, fn in os.walk(os.path.expanduser("src")) for f in fn]
    fil=[]
    for i in ext:
        fil.extend(filter(lambda x: x.endswith(i),tmp))
    return fil

def create_file_objs(files=None, macros={}):
    l=[]
    d={}

    if files is None:
        files = get_source()

    for i in files:
        source_file = file_obj()

        source_file.file_name = i
        source_file.uses = get_uses(i,macros)
        source_file.includes = get_includes(i,macros)
        source_file.contains = get_contains(i)
        source_file.is_program = get_program(i)

        d[i.lower().strip()]=source_file

        l.append(source_file)

    return l,d

def get_uses(infile=None, macros={}):
    "Return which modules are used in infile after expanding macros"
    p=re.compile("^\s*use\s*(?P<moduse>\w*)\s*(,)?\s*(only)?\s*(:)?.*?$",re.IGNORECASE).match

    uses=[]

    with open(infile,'r') as f:
        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            uses.append(tmp.group('moduse').strip())

    # Remove duplicates
    uniq_mods = list(set(uses))

    for i, mod in enumerate(uniq_mods):
        for k, v in macros.items():
            if re.match(k, mod, re.IGNORECASE):
                uniq_mods[i] = mod.replace(k,v)

    return uniq_mods

def get_includes(infile=None, macros={}):
    "Return which modules are used in infile after expanding macros"
    p=re.compile("^\s*include\s*'(?P<finclude>.*)'\s*$",re.IGNORECASE).match

    includes=[]
    dparts,fil=os.path.split(infile)

    with open(infile,'r') as f:
        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            include = tmp.group('finclude').strip()
            includes.append(os.path.join(dparts, include))

    # Remove duplicates
    uniq_mods = list(set(includes))

    for i, mod in enumerate(uniq_mods):
        for k, v in macros.items():
            if re.match(k, mod, re.IGNORECASE):
                uniq_mods[i] = mod.replace(k,v)

    return uniq_mods

def get_contains(infile=None):
    "Return all the modules that are in infile"
    p=re.compile("^\s*module\s*(?P<modname>\w*)",re.IGNORECASE).match

    contains=[]

    with open(infile,'r') as f:
        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            contains.append(tmp.group('modname').strip())

    # Remove duplicates before returning
    return list(set(contains))

def get_program(infile=None):
    "Finds if this is a program (True) or not (False)"
    p=re.compile("^\s*program\s*(?P<progname>\w*)",re.IGNORECASE).match

    contains=[]

    with open(infile,'r') as f:
        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            contains.append(tmp.group('progname').strip())

    if (not contains):
        return False
    else:
        return True

def file_objs_to_mod_dict(file_objs=[]):
    "Turn a list of file_objs in a dictionary, containing which modules depend on which files"
    dic={}
    for i in file_objs:
        for j in i.contains:
            #dic[j.lower()]=i.file_name
            dic[j.lower()]=i
    return dic

def get_all_depends(tmp,fobj,m2f,depth,root):
    if (depth > 100):
        print('Cyclic dependency likely: while processing dependencies of ' + root.file_name + \
                '. All unique files: ' + str(tmp))
        print('Goodbye.')
        sys.exit(-1)

    for j in fobj.uses:
        #try:
        if j:
            try:
                jobj = m2f[j.lower()]
            except:
                continue

            if (fobj.file_name.strip() != jobj.file_name.strip()):
                tmp.add(jobj.file_name)

                get_all_depends(tmp,jobj,m2f,depth+1,root)
        #except:
        #    pass

    for j in fobj.includes:
        #try:
        if j:
            try:
                jobj = m2f[j.lower()]
            except:
                continue

            tmp.add(jobj.file_name)

            get_all_depends(tmp,jobj,m2f,depth+1,root)
        #except:
        #    pass


def get_depends(fob=[],m2f=[],f2f=[]):
    deps={}
    for i in fob:
        tmp=set()
        for j in i.uses:
            if (j.strip()):
                try:
                    jobj = m2f[j.lower()]
                    if (i.file_name.strip() != jobj.file_name.strip()):
                        tmp.add(jobj.file_name)
                    else:
                        print(i.file_name + " depends on itself. Ignoring.")
                except:
                    print("Warning module "+j+" not defined in any files. Skipping...")

        for j in i.includes:
            if (j.strip()):
                if (os.path.isfile(j.strip())):
                    tmp.add(j.strip())
                else:
                    print("Warning include "+j+" not defined in any files. Skipping...")

        deps[i.file_name]=tmp

        if (i.is_program):
            tmp = set()
            # recursively add all dependencies for an executable
            get_all_depends(tmp,i,m2f,0,i)

            path,fil=os.path.split(i.file_name)
            exeName = os.path.join(path, fil.split(".")[0]+".exe")

            deps[exeName]=list(tmp)

    return deps

class file_obj:
    def __init__(self):
        self.file_name=None
        self.uses=None
        self.contains=None
        self.includes=None
        self.depends_on=None
        self.is_program=False

#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='Generate Fortran dependencies')
    parser.add_argument('-f','--files',nargs='+',help='Files to process')
    parser.add_argument('-D',nargs='+',action='append',metavar='NAME=DESCRIPTION',
                        help="""The macro NAME is replaced by DEFINITION in 'use' statements""")
    parser.add_argument('-b','--build',nargs=1,help='Build Directory (prepended to all files in output',
                        default='')
    parser.add_argument('-o','--output',nargs=1,help='Output file')
    parser.add_argument('-v','--verbose',action='store_true',help='explain what is done')
    parser.add_argument('-w','--overwrite',action='store_true',help='Overwrite output file without warning')

    # Parse the command line arguments
    args = parser.parse_args()

    # Assemble a dictionary out of the macro definitions
    macros = {}
    if args.D:
        for arg in args.D:
            for var in arg:
                temp = var.split('=')
            macros[temp[0]] = temp[1]

    output = args.output[0] if args.output else None
    build = args.build[0] if args.build else ''

run(files=args.files, verbose=args.verbose, overwrite=args.overwrite, macros=macros, output=output, build=build)
