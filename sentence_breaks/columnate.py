#!/usr/bin/python

import sys
import re

n=int(sys.argv[2])
if n%2 == 0:
	print "windowsize must be odd number"
	sys.exit(1)

context=(n-1)/2

columns1=[] 
columns2=[]
for i in xrange(context):
	columns1.append("NONE")
	columns2.append("CONT")

max_block_cnt=20010 # how many blocks of 10000 will we allow, for now 20000, good for ca 2e8 input examples?
block_cnt=0
cnt=0
fid=open(sys.argv[1],"r")
for line in fid.readlines():
	col1=[]
	col2=[]
	for word in line.split():
		if word == "|" and col2:
			col2[-1]="STOP"
		elif re.search("_",word):
			col1.append(word.split("_")[-1].replace(",","<komma>"))
			col2.append("CONT")
	columns1 = columns1 + col1
	columns2 = columns2 + col2

	nr=len(columns2)
	if nr>10000:
		block_cnt=block_cnt+1
		ndx=nr-1
		# find last instance of STOP with enough right context left to fill a window
		while not (columns2[ndx]=="STOP" and nr-1-ndx>=context):
			ndx=ndx-1

		for x in xrange(context,ndx+1):
			cnt=cnt+1
			wo=""
			for a in xrange(n):
				wo=wo+columns1[x-context+a]+","
			print "{0},{1}".format(wo[:-1],columns2[x])
			if columns2[x]=="STOP":
				cnt=0

		columns1=columns1[ndx+1-context:]
		columns2=columns2[ndx+1-context:]
	if block_cnt==max_block_cnt:
		break
fid.close()

nr=len(columns2)
ndx=nr-1
for i in xrange(context):
	columns1.append("NONE")
	columns2.append("CONT")
for x in xrange(context,ndx+1):
	cnt=cnt+1
	wo=""
	for a in xrange(n):
		wo=wo+columns1[x-context+a]+","
	print "{0},{1}".format(wo[:-1],columns2[x])
	if columns2[x]=="STOP":
		cnt=0
