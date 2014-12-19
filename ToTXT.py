import csv
with open('/Users/apple/Downloads/essays.csv','rb') as csvfile:
    reader= csv.reader(csvfile,delimiter=',',quotechar='"')
    i=0
    for row in reader:
        i+=1
        temptext=row
        tempf=open("/Volumes/Bootcamp/BigData/txt/essay"+str(row[0])+".txt",'w')
        tempf.write(row[5])
