#!/usr/bin/python

import sys
import copy as cp
from math import floor, ceil, sqrt


class Word:
	def __init__(self,nstr,nstart,nstop):
		self.str=nstr
		self.start=nstart
		self.stop=nstop
	def __eq__(self,other):
		return self.str==other.str and self.start==other.start and self.stop==other.stop
	def __ne__(self,other):
		return self.str!=other.str or self.start!=other.start or self.stop!=other.stop

class Token:
	def __init__(self,nlength=0,npath=[],ncost=float("inf")):
		self.length=nlength
		self.path=npath
		self.cost=ncost

def quick_endrange_compare(seq,beginning,strt,stp,fixed):
	strt=max(strt,beginning+2)
	stp=min(stp,len(seq))

	fix_voc={}
	seq_voc={}
	seq_norm=0
	fix_norm=0
	seq_norms=[]
	similarities=[]
	similarity=0
	indices=[]
	for x in xrange(len(fixed)-1):
		w=fixed[x]+"|"+fixed[x+1]
		if w in fix_voc:
			fix_norm-=fix_voc[w]*fix_voc[w]
			fix_voc[w]+=1
			fix_norm+=fix_voc[w]*fix_voc[w]
		else:
			fix_voc[w]=1
			fix_norm+=1
	for x in xrange(beginning,strt-2):
		w=seq[x]+"|"+seq[x+1]
		if w in seq_voc:
			seq_norm-=seq_voc[w]*seq_voc[w]
			seq_voc[w]+=1
			seq_norm+=seq_voc[w]*seq_voc[w]
		else:
			seq_voc[w]=1
			seq_norm+=1
		if w in fix_voc:
			similarity+=fix_voc[w]
	for x in xrange((strt-2),(stp-1)):
		w=seq[x]+"|"+seq[x+1]
		if w in seq_voc:
			seq_norm-=seq_voc[w]*seq_voc[w]
			seq_voc[w]+=1
			seq_norm+=seq_voc[w]*seq_voc[w]
		else:
			seq_voc[w]=1
			seq_norm+=1
		if w in fix_voc:
			similarity+=fix_voc[w]
		similarities.append(float(similarity)/(1e-15+sqrt(seq_norm)*sqrt(fix_norm)))
		indices.append(x+2)
		# seq_norms.append(seq_norm)
	return [similarities,indices]

def tune_boundaries(wlist,bnd0,bnd1,line):
	l=len(line)
	strt=bnd1-int(0.9*l)
	stp=bnd1+int(0.9*l)
	[x,n]=quick_endrange_compare(wlist,bnd0,strt,stp,line)
	m=max(x)
	ndx=[i for i,j in enumerate(x) if j==m][0]
	return (n[ndx],m)

def make_alignment(sequence1,sequence2):
	m=len(sequence1)+1
	n=len(sequence2)+1

	order=[]
	for k in xrange(m+n):
		for b in xrange(n):
			a=k-b
			if a<m and a>=0: 
				order.append([a,b])

	tokens={}
	tokens[0,0]=Token()
	tokens[0,0].cost=0
	ntokens=1

	done=False
	end_token_paths=[[] for x in xrange(n)]
	end_token_costs=[[] for x in xrange(n)]
	for i,j in [(order[x][0],order[x][1]) for x in xrange(len(order))]:
		if (i,j) in tokens:
			# insertion
			if j<(n-1):
				if not (i,j+1) in tokens:
					ntokens+=1
					tokens[i,j+1]=Token(tokens[i,j].length+1,tokens[i,j].path+[[i,j]],tokens[i,j].cost+1)
				elif ((tokens[i,j].cost+1 < tokens[i,j+1].cost) or
						((tokens[i,j].cost+1==tokens[i,j+1].cost) and 
						tokens[i,j].length+1<tokens[i,j+1].length)):
					tokens[i,j+1]=Token(tokens[i,j].length+1,tokens[i,j].path+[[i,j]],tokens[i,j].cost+1)
				if i==m-1 and j==n-2:
					end_token_path=tokens[i,j+1].path + [[i,j+1]]
					end_token_cost=tokens[i,j+1].cost
			# deletion
			if i<(m-1):
				if not (i+1,j) in tokens:
					ntokens+=1
					tokens[i+1,j]=Token(tokens[i,j].length+1,tokens[i,j].path+[[i,j]],tokens[i,j].cost+1)
				elif ((tokens[i,j].cost+1 < tokens[i+1,j].cost) or
						((tokens[i,j].cost+1==tokens[i+1,j].cost) and 
						tokens[i,j].length+1<tokens[i+1,j].length)):
					tokens[i+1,j]=Token(tokens[i,j].length+1,tokens[i,j].path+[[i,j]],tokens[i,j].cost+1)
				if i==m-2 and j==n-1:
					end_token_path=tokens[i+1,j].path + [[i+1,j]]
					end_token_cost=tokens[i+1,j].cost
			# substitution
			if i<(m-1) and j<(n-1):
				if sequence1[i]==sequence2[j]:
					c=0
				else:
					c=1

				if not (i+1,j+1) in tokens:
					ntokens+=1
					tokens[i+1,j+1]=Token(tokens[i,j].length+1,tokens[i,j].path+[[i,j]],tokens[i,j].cost+c)
				elif ((tokens[i,j].cost+c < tokens[i+1,j+1].cost) or
						((tokens[i,j].cost==tokens[i+1,j+1].cost) and 
						tokens[i,j].length+1<tokens[i+1,j+1].length)):
					tokens[i+1,j+1]=Token(tokens[i,j].length+1,tokens[i,j].path+[[i,j]],tokens[i,j].cost+c)
				if i==m-2 and j==n-2:
					end_token_path=tokens[i+1,j+1].path + [[i+1,j+1]]
					end_token_cost=tokens[i+1,j+1].cost

			del tokens[i,j]
			ntokens-=1

			if ntokens>200:
				#find 200 worst tokens and throw them out
				coords=[]
				c=[]
				for i,j in tokens:
					coords.append([i,j])
					c.append(tokens[i,j].cost)
				ndx=range(len(c))
				ndx=sorted(ndx, key=lambda p: tokens[coords[p][0],coords[p][1]].cost,reverse=True)
				for x in xrange(100):
					del tokens[coords[ndx[x]][0],coords[ndx[x]][1]]
				ntokens-=100

	# write the lineup
	lineup=[]
	ref_lineup=[]
	for x in xrange(len(end_token_path),0,-1):
		a=end_token_path[x-1][0]
		b=end_token_path[x-1][1]
		lineup.append(a)
		ref_lineup.append(b)
	lineup.reverse()
	ref_lineup.reverse()
	return [lineup,ref_lineup,end_token_cost]

def write_out(key,word_list):
	print ";; {0}_seg{1:0>8d}:{2:0>8d} {0} {3} {4:.3f}".format(key,int(100*float(word_list[0].start)),int(100*(float(word_list[-1].start)+float(word_list[-1].stop))),word_list[0].start,float(word_list[-1].start)+float(word_list[-1].stop))

	for x in xrange(len(word_list)):
		print "{0} 1 {1} {2} {3}".format(key,word_list[x].start,word_list[x].stop,word_list[x].str)

# read original ctm
fid=open(sys.argv[2],'r')
wlist=[]
for line in fid.readlines():
	f=line.split()
	fkey=f[0]
	temp_word=Word(f[4],f[2],f[3])
	wlist.append(temp_word)
fid.close()
wslist=[x.str for x in wlist]

# read line by line the split text
lines=[]
tlines=[]
nlines=0
fid=open(sys.argv[1],'r')
for line in fid.readlines():
	[word,tag]=line.split()
	tlines.append(word)
	if tag=="STOP":
		lines.append(tlines)
		tlines=[]
fid.close()
if tlines:
	lines.append(tlines)

bnd0=0
ref1=lines[0]
bnd1=bnd0+len(ref1)
[bnd1_opt,score]=tune_boundaries(wslist,bnd0,bnd1,ref1)

for x in xrange(1,len(lines)):
	ref2=lines[x]
	bnd2=bnd1_opt+len(ref2)
	[bnd2_opt,score]=tune_boundaries(wslist,bnd1_opt,bnd2,ref2)

	snippet=wslist[bnd0:min(len(wslist),bnd2_opt+50)]
	[l1,l2,c]=make_alignment(ref1+ref2,snippet)
	ndx=(len(l1)-1)-l1[::-1].index(len(ref1))
	bnd1_real=bnd0+l2[ndx]

	write_out(fkey,wlist[bnd0:bnd1_real])
	bnd0=bnd1_real
	bnd1_opt=bnd2_opt
	ref1=ref2
fid.close()
write_out(fkey,wlist[bnd0:])

