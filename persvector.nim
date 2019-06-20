## This module implements Clojures persistent vectors with tail optimisation.
## For more information about those please see the blogpost series:
## http://hypirion.com/musings/understanding-persistent-vector-pt-1 which
## was used as a reference for this implementation.
##
## Persistent, or immutable, data structures are important for many functional
## workloads. Instead of changing the data within them they have a structure
## which shares nodes with similar data so a minimal amount of data needs to
## be copied. This is not however only used for functional programming and have
## benefits for things like asynchronous programming and even systems which
## stores it's state to provide things like an undo feature.

const persvectorbits {.intdefine.}: int = 5

const
  ## These constants define how the node sharing is done. For Clojure BITS is
  ## set to 5, which gives 32 elements per node. Fewer bits means less copied
  ## data but a deeper tree and more work in maintaining it. More bits means
  ## more copied data but a shallower tree and less work. 32 is seen to be a
  ## good compromise between memory and computational performance.
  BITS = persvectorbits
  WIDTH = 1 shl BITS
  MASK = WIDTH - 1


type
  NodeKind = enum
    leaf, branch
  VectorNode[T] = ref VectorNodeObj[T]
  VectorNodeObj[T] = object
    case kind: NodeKind
    of leaf:
      data: seq[T]
    of branch:
      children: seq[VectorNode[T]]
  PersistentVector*[T] = ref object
    tail: seq[T]
    size: int
    shifts: int
    tree: VectorNode[T]

func copyRef[T](theSeq: seq[T]): seq[T] =
  shallowCopy(result, theSeq)


func initVector*[T](): PersistentVector[T] =
  new result
  result.tail = @[]

func add*[T](vec: PersistentVector[T], elem: T): PersistentVector[T] =
  ## Returns a new persistent vector with the element `elem` inserted at the end
  new result
  result.size = vec.size + 1
  result.shifts = vec.shifts
  if vec.tail.len != WIDTH:
    result.tail.deepCopy vec.tail
    result.tail.add elem
    result.tree = vec.tree
  else:
    if vec.tree == nil:
      result.tree = VectorNode[T](kind: leaf, data: copyRef(vec.tail))
    else:
      if vec.tree.kind == leaf:
        result.tree = VectorNode[T](kind: branch, children: @[vec.tree])
        var newChild = VectorNode[T](kind: leaf, data: copyRef(vec.tail))
        result.tree.children.add newChild
        result.shifts = BITS
      else:
        var n = vec.size - vec.tail.len
        while (n and (WIDTH - 1)) == 0:
          n = n shr BITS
        if n == 1:
          result.tree = VectorNode[T](kind: branch, children: @[vec.tree])
          var
            level = vec.shifts
            node = result.tree
          result.shifts = 0
          while level > 0:
            var nnode = VectorNode[T](kind: branch, children: @[])
            node.children.add nnode
            node = nnode
            level -= BITS
            result.shifts += BITS
          result.shifts += BITS
          node.children.add VectorNode[T](kind: leaf, data: copyRef(vec.tail))
        else:
          result.tree = VectorNode[T](kind: branch)
          result.tree.children = vec.tree.children[0 .. ^1]
          var
            node = result.tree
            level = vec.shifts
          while level > 0:
            let index = ((vec.size - vec.tail.len) shr level) and MASK
            if node.children.high < index:
              while level > BITS:
                level -= BITS
                node.children.add VectorNode[T](kind: branch, children: @[])
                node = node.children[node.children.high]
              node.children.add VectorNode[T](kind: leaf, data: copyRef(vec.tail))
              break
            else:
              let oldNode = node.children[index]
              node.children[index] = VectorNode[T](kind: branch)
              node = node.children[index]
              node.children = oldNode.children[0 .. ^1]
            level -= BITS

    result.tail = @[elem]

func update*[T](vec: PersistentVector[T], key: int, elem: T): PersistentVector[T] =
  ## Returns a new persistent vector with the element at `key` changed to `elem`
  new result
  result.size = vec.size
  result.shifts = vec.shifts
  if key >= vec.size - vec.tail.len:
    result.tail = vec.tail[0 .. ^1]
    result.tree = vec.tree
    result.tail[key - (vec.size - vec.tail.len)] = elem
  else:
    result.tail = vec.tail
    result.tree = VectorNode[T](kind: vec.tree.kind)
    if result.tree.kind == leaf:
      result.tree.data = vec.tree.data[0 .. ^1]
      result.tree.data[key] = elem
    else:
      result.tree.children = vec.tree.children[0 .. ^1]
      var
        node = result.tree
        level = vec.shifts
      while level > 0:
        let
          index = (key shr level) and MASK
          oldNode = node.children[index]
        node.children[index] = VectorNode[T](kind: oldNode.kind)
        node = node.children[index]
        if oldNode.kind == branch:
          node.children = oldNode.children[0 .. ^1]
        else:
          node.data = oldNode.data[0 .. ^1]
        level -= BITS
      node.data[key and MASK] = elem

func delete*[T](vec: PersistentVector[T]): PersistentVector[T] =
  ## Returns a new persistent vector with the last element of the given vector missing.
  new result
  result.size = vec.size - 1
  if vec.tail.len > 1 or vec.tree == nil:
    result.shifts = vec.shifts
    result.tree = vec.tree
    result.tail = vec.tail[0 .. ^2]
  else:
    if vec.tree.kind == leaf:
      result.tree = nil
      result.tail = copyRef(vec.tree.data)
    else:
      var n = result.size - WIDTH
      while (n and (WIDTH - 1)) == 0:
        n = n shr BITS
      # If new size of tree is power of WIDTH, the right branch of the tree only consists of one node to be promoted.
      if n == 1:
        result.tree = vec.tree.children[0]
        var node = vec.tree.children[1]
        while node.kind != leaf:
          node = node.children[0]
        result.tail = copyRef(node.data)
        result.shifts = vec.shifts - BITS
      else:
        result.shifts = vec.shifts
        proc promoteRight[T](vector: PersistentVector[T], node: VectorNode[T]): VectorNode[T] =
          if node.kind == branch:
            let newNode = promoteRight(vector, node.children[node.children.high])
            if newNode == nil and node.children.len == 1:
              return nil
            result = VectorNode[T](kind: branch, children: node.children[0 .. ^2])
            if newNode != nil:
              result.children.add newNode
          else:
            vector.tail = copyRef(node.data)
            result = nil
        result.tree = promoteRight(result, vec.tree)

func toPersistentVector*[T](s: seq[T]): PersistentVector[T] =
  ## Returns a new persistent vector that contains all elements in the passed sequence. This copies all the data from the sequence.
  result = PersistentVector[T](size: s.len)
  var nodes = newSeq[VectorNode[T]](s.len shr BITS)
  result.tail = s[s.len - (s.len and MASK) .. ^1]
  for i in 0..nodes.high:
    nodes[i] = VectorNode[T](kind: leaf, data: s[WIDTH*i .. WIDTH*(i+1)-1])
  while nodes.len > WIDTH:
    result.shifts += BITS
    var newNodes = newSeq[VectorNode[T]](nodes.len shr BITS + (if (nodes.len and MASK) != 0: 1 else: 0))
    for j in 0..<newNodes.high:
      newNodes[j] = VectorNode[T](kind: branch, children: nodes[j*WIDTH .. WIDTH*(j+1)-1])
    newNodes[newNodes.high] = VectorNode[T](kind:branch, children: nodes[newNodes.high*WIDTH .. nodes.high])
    nodes = newNodes
  case nodes.len:
  of 0: result.tree = nil
  of 1: result.tree = nodes[0]
  else:
    result.shifts += BITS
    result.tree = VectorNode[T](kind: branch, children: nodes)

func `[]`*[T](vec: PersistentVector[T], key: int): T {.inline.} =
  ## Access operator for persistent vectors
  if key >= vec.size - vec.tail.len:
    return vec.tail[key - (vec.size - vec.tail.len)]
  var
    level = vec.shifts
    node = vec.tree
  while level > 0:
    let index = (key shr level) and MASK
    node = node.children[index]
    level -= BITS
  return node.data[key and MASK]

func `[]`*[T](vec: PersistentVector[T], slice: Slice[int]): seq[T] {.inline.} =
  ## Optimised slice operator for persistent vectors, returns a sequence
  if slice.a == slice.b:
    return @[vec[slice.a]]
  elif slice.a >= vec.size - vec.tail.len:
    return vec.tail[slice.a - (vec.size - vec.tail.len) .. slice.b - (vec.size - vec.tail.len)]
  else:
    var i = slice.a
    let e = min(slice.b, vec.size - vec.tail.len - 1)
    result = newSeq[T](slice.b - slice.a + 1)
    while i <= e:
      var
        level = vec.shifts
        node = vec.tree
      while level > 0:
        let index = (i shr level) and MASK
        node = node.children[index]
        level -= BITS
      let start:int = i and MASK
      for d in node.data[start .. node.data.high]:
        result[i-slice.a] = d
        i += 1
        if i > e:
          break
    if i <= slice.b:
      result[i-slice.a .. slice.b-slice.a] =  vec.tail[i - (vec.size - vec.tail.len) .. slice.b - (vec.size - vec.tail.len)]

iterator pairs*[T](vec: PersistentVector[T]): tuple[key: int, val: T] {.noSideEffect.} =
  ## Optimised iterator for PersistentVector (could be optimised further)
  var i = 0
  if vec.tree != nil:
    while i < vec.size - vec.tail.len:
      var
        level = vec.shifts
        node = vec.tree
      while level > 0:
        let index = (i shr level) and MASK
        node = node.children[index]
        level -= BITS
      for j, d in node.data:
        yield (key: i + j, val: d)
      i += WIDTH
  for j, d in vec.tail:
    yield (key: i + j, val: d)

iterator items*[T](vec: PersistentVector[T]): T {.noSideEffect.} =
  ## Optimised iterator for PersistentVector (could be optimised further)
  for idx, val in vec:
    yield val

func len*[T](vec: PersistentVector[T]): int =
  ## Function to get length of a persistent vector (stored result, not calculated)
  return vec.size

func high*[T](vec: PersistentVector[T]): int =
  ## Function to get the highest valid index of the persistent vector
  return vec.size - 1

func `$`*[T](vec: PersistentVector[T]): string =
  ## Returns a string representation of the elements in the persistent vector.
  result = "PersistentVector["
  for idx, x in vec:
    result &= $x
    if idx != vec.high:
      result &= ", "
  result &= "]"

func `$`*(node: VectorNode): string =
  ## Returns a string representation of a vector node, for debugging.
  if node.kind == leaf:
    return "l" & $cast[int](node.data) & ": " & $node.data
  else:
    return "b" & $cast[int](node) & ": " & $node.children

