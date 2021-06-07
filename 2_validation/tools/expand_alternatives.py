import os

def expandLine(text):
    a,b = text.strip().split("\t")
    a = list(a.split('/'))
    b = b.split('/')
    combined = [(x,y) for x in a for y in b]
    return ['\t'.join(c) + "\n" for c in combined]

for root, dirs, files in os.walk("."):
    for filename in files:
        if filename.endswith("txt"):
            f = open(filename, 'r')
            lines = f.readlines()
            f.close()

            # store the category to write to top of file
            newlines = [lines[0]]

            [newlines.extend(expandLine(line)) for line in lines[1:]]

            newfile = os.path.join("new", filename)
            f = open(newfile, 'w')
            f.writelines(newlines)
            f.close()
