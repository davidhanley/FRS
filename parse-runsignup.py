import sys

lines = open(sys.argv[1]).readlines()
lines = [ line.strip() for line in lines ]

for i in range(len(lines)):
  line = lines[i]
  line = line.split("\t")
  #print(line)
  if len(line)>=4:
    name = lines[i-2]+" "+lines[i-1]
    gender = line[0]
    age = line[2]
    time = line[-2]

    print(",".join(["",name,age,gender,time]))



