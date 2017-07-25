## This is a file which shows most of the implemented features of the
## persistent vector data structure. It was created to test the algorithms
## during the implementation

import persvector

# Create an empty vector of ints and a sequence of vectors
var vec = initVector[int]()
var vecs = @[vec]

# Add numbers to the vector and store the produced vectors in the sequence. Note that this was tested with a node size of 2 or 4 to increase the branching. 64 numbers only ever branches once.
#for i in 0..1_000_000:
for i in 0..64:
  vecs.add(vecs[vecs.high].add i)

# Add some vectors which delete a field
vecs.add vecs[vecs.high].delete
vecs.add vecs[vecs.high].delete

# Add a vector that has a field updated:
vecs.add vecs[vecs.high-5].update(10, 100)

# Grab a slice from the vector (Note that this returns a regular sequence)
echo vecs[vecs.high][20..30]

# Create a completely new vector created from a sequence
var strVec = @["Hello", "world!", "How", "is", "it", "going?", "Persistent", "vectors", "are", "cool!"].toPersistentVector
# And create a new update of it
var neatVec = strVec.update(strVec.len-1, "neat!")

for nvec in vecs:
  echo "len: " & $nvec.len
  echo $nvec
  echo "---"

echo "len: " & $strVec.len
echo $strVec
echo "---"

echo "len: " & $neatVec.len
echo $neatVec


#echo getOccupiedMem()


