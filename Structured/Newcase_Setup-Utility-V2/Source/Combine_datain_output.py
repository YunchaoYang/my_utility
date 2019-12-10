#! /share/opt/python/2.7.3/bin/python


files = ['bcoutputbc.datain','bcoutputwall.datain','bcoutput7.datain']
#read_files = glob.glob(files)

with open("datain.bc", "wb") as outfile:
    for f in files:
        with open(f, "rb") as infile:
            outfile.write(infile.read())
    outfile.write("&bcdef bcdir='end'/ \n")
    outfile.write("&bcwake wbcdir='end'/ \n")
    outfile.write("&spec_output optype = 'end'/ \n")
