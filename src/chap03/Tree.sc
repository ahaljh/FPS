import chap03.Tree
import chap03.{Tree,Leaf,Branch}
import chap03.Tree._

val leaf = Leaf(1)
val leaf2 = Leaf(2)
val leaf3 = Leaf(3)
val leaf4 = Leaf(4)
val branch1 = Branch(leaf, leaf2)
val branch2 = Branch(leaf3, leaf4)
val root = Branch(branch1, branch2)

// Example 3.25
size(leaf)
size(branch1)
size(root)

// Example 3.26
maximum(root)
maximum(branch1)

// Example 3.27
depth(root)
depth(branch1)
depth(leaf)

// Example 3.28
map(root)(x => x+1)

// Example 3.29
size2(leaf)
size2(branch1)
size2(root)

maximum2(root)
maximum2(branch1)

depth(root)
depth(branch1)
depth(leaf)

map2(root)(x => x+1)