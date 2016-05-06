import hashlib
import csv
import sys

with open(file, "r") as infile, open("hashed_" + file, "w") as outfile:
    drows = csv.DictReader(infile)
    rows = list(drows)
    for row in rows: 
        hasher = hashlib.sha224()
        hasher.update(row["workerId"].encode())
        row["workerId"] = hasher.hexdigest() 

    a = csv.DictWriter(outfile, fieldnames=drows.fieldnames)
    a.writeheader()
    a.writerows(rows)
