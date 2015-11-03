#!/usr/bin/env python

import re
import csv
import codecs

MULTI_DELIM = ";;"
RIS_PAT = re.compile('(\w\w)  -(.*)$')

class RISException(Exception):
    pass

def ris_line(line):
    text = line.strip()
    if text == "":
        return None,None
    m = RIS_PAT.match(text)
    if not m:
        raise RISException("Problem parsing line: " + text)
    # strip leading whitespace on the field value
    return m.group(1),m.group(2).strip()


def ris_fields(f):
    fields = set()
    for line in f:
        key,val = ris_line(line)
        if key is not None and key != 'ER':
            fields.add(key)
    return fields

def ris2csv(f):
    entries = []
    data = dict()
    
    for line in f:
        key,val = ris_line(line)
        if key is None:
            continue
        if key == 'ER':
            # done reading previous entry; start new one
            entries.append(data)
            data = dict()
            continue
        if key == 'TY' and len(data) > 0:
            raise RISException("New entry does not start with TY: " + line)
            continue
        # RIS allows repeating the same data field multiple times
        if key in data:
            data[key] += MULTI_DELIM + val
        else:
            data[key] = val

    return entries


def main(files,output):
    fields = set()
    for filename in files:
        with codecs.open(filename, "r", encoding="utf-8") as f:
            fields = fields.union(ris_fields(f))

    headers = sorted(fields)
    w = csv.DictWriter(output,headers,quoting=csv.QUOTE_ALL)
    w.writeheader()
            
    for filename in files:
        with open(filename) as f:
            for row in ris2csv(f):
                for k, v in row:
                    if isinstance(v, unicode):
                        row[k] = v.encode('utf8')
                w.writerow(row)

if __name__=='__main__':
    import sys
    main(sys.argv[1:],sys.stdout)
