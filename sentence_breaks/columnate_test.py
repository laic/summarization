#!/usr/bin/python

import sys
import re

if len(sys.argv)==4:
	wordfile=sys.argv[3]
	wordfid=open(wordfile,"w")
else:
	wordfile=None

n=int(sys.argv[2])
if n%2 == 0:
	print "windowsize must be odd number"
	sys.exit(1)

context=(n-1)/2

columns1=[] 
columns2=[]
columns3=[]
for i in xrange(context):
	columns1.append("NONE")
	columns2.append("CONT")

fid=open(sys.argv[1],"r")
for line in fid.readlines():
	col1=[]
	col2=[]
	col3=[]
	for word in line.split():
		if re.search("_",word):
			col1.append(word.split("_")[-1].replace(",","<komma>"))
			col2.append("CONT")
			col3.append(word.split("_")[:-1])
	columns1 = columns1 + col1
	columns2 = columns2 + col2
	columns3 = columns3 + col3

	nr=len(columns2)
	if nr>10000:
		ndx=nr-1-context

		for x in xrange(context,ndx+1):
			wo=""
			for a in xrange(n):
				wo=wo+columns1[x-context+a]+","
			print "{0},{1}".format(wo[:-1],columns2[x])
			if wordfile:
				wordfid.write('{0}\n'.format(columns3[x-context][0]))

		columns1=columns1[ndx+1-context:]
		columns2=columns2[ndx+1-context:]
		columns3=columns3[ndx+1-context:]

fid.close()

nr=len(columns2)
ndx=nr-1
for i in xrange(context):
	columns1.append("NONE")
	columns2.append("CONT")
for x in xrange(context,ndx+1):
	wo=""
	for a in xrange(n):
		wo=wo+columns1[x-context+a]+","
	print "{0},{1}".format(wo[:-1],columns2[x])
	if wordfile:
		wordfid.write('{0}\n'.format(columns3[x-context][0]))
