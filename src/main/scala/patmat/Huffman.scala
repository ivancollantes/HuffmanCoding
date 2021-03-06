package patmat

import common._

import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
    abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
    def weight(tree: CodeTree): Int = tree match {
      case Fork(left, right, chars, weight) => weight
      case Leaf(char, weight) => weight
    }
  
    def chars(tree: CodeTree): List[Char] = tree match {
      case Fork(left, right, chars, weight) => chars
      case Leaf(char, weight) => List(char)
    }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
    def times(chars: List[Char]): List[(Char, Int)] = {
      @tailrec
      def inner(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
        if(chars.isEmpty) acc
        else {
          val currentChar: Char = chars.head
          val currentCharList: List[Char] = chars.filter(char => char == currentChar)
          val otherCharList: List[Char] = chars.filter(char => char != currentChar)
          inner(otherCharList, (currentChar, currentCharList.length) :: acc)
        }
      }
      inner(chars, List())
    }
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
      @tailrec
      def convertToLeaf(pairs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] = {
        if(pairs.isEmpty) acc
        else convertToLeaf(pairs.tail, Leaf(pairs.head._1, pairs.head._2) :: acc)
      }
      convertToLeaf(freqs, List()).sortWith(_.weight < _.weight)
    }
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
    def singleton(trees: List[CodeTree]): Boolean = trees.length == 1
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
    def combine(trees: List[CodeTree]): List[CodeTree] = {
      if(trees.length < 2) trees
      else {
        val newFork: CodeTree = makeCodeTree(trees.head, trees.tail.head)
        val remainingTree: List[CodeTree] = trees.tail.tail
        val newTree: List[CodeTree] = newFork :: remainingTree
        newTree.sortWith((left: CodeTree, right: CodeTree) => {
          (left, right) match {
            case (left: Fork, right: Fork) => left.weight < right.weight
            case (left: Leaf, right: Leaf) => left.weight < right.weight
            case (left: Fork, right: Leaf) => left.weight < right.weight
            case (left: Leaf, right: Fork) => left.weight < right.weight
          }
        })
      }
    }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
    def until(limit: List[CodeTree] => Boolean, reductor: List[CodeTree] => List[CodeTree])(list: List[CodeTree]): CodeTree = {
      if(limit(list)) list.head
      else until(limit, reductor)(reductor(list))
    }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
    def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))
  

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
      @tailrec
      def decodeBit(subTree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
        if(bits.isEmpty) acc
        else {
          subTree match {
            case leaf: Leaf => decodeBit(tree, bits, acc ::: List(leaf.char))
            case fork: Fork => bits.head match {
              case 0 => decodeBit(fork.left, bits.tail, acc)
              case 1 => decodeBit(fork.right, bits.tail, acc)
            }
          }
        }
      }
      /* I add a dummy bit at the end of the bits list so that execution ends nicely */
      decodeBit(tree, bits ::: List(0), List())
    }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
    val rrrl: CodeTree = Leaf('i',115465) // i: 1110
    val rrrr: CodeTree = Leaf('a',117110) // a: 1111
    val rrr: CodeTree = Fork(rrrl,rrrr,List('i','a'),232575)
    val rrl: CodeTree = Leaf('e',225947) // e: 110
    val rr: CodeTree = Fork(rrl, rrr, List('e','i','a'),458522)
    val rlrr: CodeTree = Leaf('t',111103) // t: 1011
    val rlrl: CodeTree = Leaf('n',108812) // n: 1010
    val rlr: CodeTree = Fork(rlrl,rlrr,List('n','t'),219915)
    val rllrrrr: CodeTree = Leaf('b',13822) // b: 1001111
    val rllrrrl: CodeTree = Leaf('g',13288) // g: 1001110
    val rllrrr: CodeTree = Fork(rllrrrl,rllrrrr,List('g','b'),27110)
    val rllrrl: CodeTree = Leaf('v',24975) // v: 100110
    val rllrr: CodeTree = Fork(rllrrl, rllrrr, List('v','g','b'),52085)
    val rllrl: CodeTree = Leaf('c',50003) // c: 10010
    val rllr: CodeTree = Fork(rllrl, rllrr, List('c','v','g','b'),102088)
    val rlll: CodeTree = Leaf('r',100500) // r: 1000
    val rll: CodeTree = Fork(rlll, rllr, List('r','c','v','g','b'),202588)
    val rl: CodeTree = Fork(rll, rlr, List('r','c','v','g','b','n','t'),422503)
    val r: CodeTree = Fork(rl, rr, List('r','c','v','g','b','n','t','e','i','a'),881025)

  val l: CodeTree =
    Fork(
      Fork(
        Leaf('s',121895),
        Fork(
          Leaf('d',56269),
          Fork(
            Fork(
              Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),
              Leaf('f',16351),List('x','j','f'),30630),
            Fork(
              Fork(
                Fork(
                  Fork(
                    Leaf('z',2093),
                    Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),
                  Leaf('y',4725),List('z','k','w','y'),9310),
                Leaf('h',11298),List('z','k','w','y','h'),20608),
              Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),
      Fork(
        Fork(
          Leaf('o',82762),
          Leaf('l',83668),List('o','l'),166430),
        Fork(
          Fork(
            Leaf('m',45521),
            Leaf('p',46335),List('m','p'),91856),
          Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362)

  val frenchCode: CodeTree = Fork(l, r, List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the 'frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
    def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
      /* I suppose there are no special characters, only a-zA-Z */
      def treeContainsChar(tree: CodeTree, char: Char): Boolean = {
        tree match {
          case leaf: Leaf => leaf.char == char
          case fork: Fork => fork.chars.contains(char)
        }
      }
      @tailrec
      def encodeChar(tree: CodeTree, char: Char, acc: List[Bit]): List[Bit] = {
        tree match {
          case leaf: Leaf => acc
          case fork: Fork => {
            if(treeContainsChar(fork.left, char)) encodeChar(fork.left, char, acc ::: List(0))
            else encodeChar(fork.right, char, acc ::: List(1))
          }
        }
      }
      text.foldLeft(List[Bit]()) {
        (acc: List[Bit], char: Char) =>
          acc ::: encodeChar(tree, char, List[Bit]())
      }
    }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
    def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(_._1 == char).head._2
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
    def convert(tree: CodeTree): CodeTable = {
      def branchContainsChar(tree: CodeTree, char: Char): Boolean = {
        tree match {
          case leaf: Leaf => leaf.char == char
          case fork: Fork => fork.chars.contains(char)
        }
      }
      def encodeChar(tree: CodeTree, char: Char, acc: CodeTable): CodeTable = {
        (tree match {
          case leaf: Leaf => acc
          case fork: Fork => {
            if(branchContainsChar(fork.left, char)) encodeChar(fork.left, char, mergeCodeTables(List((char, List(0))), acc))
            else encodeChar(fork.right, char, mergeCodeTables(List((char, List(1))), acc))
          }
        }).reverse
      }
      tree match {
        case leaf: Leaf => List()
        case fork: Fork => fork.chars.foldLeft(List[(Char, List[Bit])]()) {
          (acc: CodeTable, char: Char) => mergeCodeTables(encodeChar(fork, char, List[(Char, List[Bit])]()), acc)
        }
      }
    }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
      a.foldLeft(b) { (acc: CodeTable, pair: (Char, List[Bit])) => {
        // acc = b
        if(acc.exists(_._1 == pair._1)) acc.map((innerPair: (Char, List[Bit])) => {
          if(innerPair._1 == pair._1) (innerPair._1, innerPair._2 ::: pair._2)
          else innerPair
        })
        else pair :: acc
      }} sortWith(_._1 < _._1)
    }
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
    // curried version (type is CodeTree => List[Char] => List[Bit])
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text.flatMap(codeBits(convert(tree)))
  }

//List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'b', 'c', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.') did not equal
//List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.')