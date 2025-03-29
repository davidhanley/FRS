import sys

lines = open(sys.argv[1]).readlines()
lines = [ line.strip() for line in lines ]

for i in range(len(lines)):
  linestr = lines[i]
  line = linestr.split("\t")
  #print(line)
  if ":" in linestr:
    name = lines[i-2]+" "+lines[i-1]
    gender = line[2]
    age = line[3]
    time = line[0]

    print(",".join(["",name,age,gender,time]))



