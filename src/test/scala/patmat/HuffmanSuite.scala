package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times creates a list of pairs") {
    val expectedList: List[(Char, Int)] = List(('k', 1), ('d', 1), ('b', 3), ('a', 5))
    val resultingList = times(string2Chars("abababadak"))
    assert(resultingList === expectedList)
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))

    val leaflist2 = List(Leaf('e', 3), Leaf('t', 4), Leaf('x', 5))
    assert(combine(leaflist2) === List(Leaf('x',5), Fork(Leaf('e',3),Leaf('t',4),List('e', 't'),7)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


  test("mergeCodeTables returns a merged CodeTable") {
    val codeTableLeft = List(('b', List(0)))
    val codeTableRight = List(('c', List(0)), ('d', List(1)))
    val expectedMergedCodeTable = List(('b', List(0)), ('c', List(0)), ('d', List(1)))
    val expectedMergedCodeTableDoubleB = List(('b', List(0, 0)), ('c', List(0)), ('d', List(1)))
    assert(mergeCodeTables(codeTableLeft, codeTableRight) === expectedMergedCodeTable)
    assert(mergeCodeTables(codeTableLeft, expectedMergedCodeTable) === expectedMergedCodeTableDoubleB)
  }

  test("decode decodes secret and encode gets the original encoded secret") {
    val decodedSecretString = decodedSecret.mkString
    assert(encode(frenchCode)(string2Chars(decodedSecretString)) === secret)
  }

  test("decode and encode work as expected") {
    val secretMessage: String = "berria"
    val secretMessaheChars: List[Char] = string2Chars(secretMessage)
    val encoded: List[Bit] = encode(frenchCode)(secretMessaheChars)
    val decoded: List[Char] = decode(frenchCode, encoded)
    val decodedMessage: String = decoded.mkString
    assert(decodedMessage === secretMessage)
  }

  test("quickEncode encodes the same as encode") {
    val secretMessage: String = "berria"
    val secretMessaheChars: List[Char] = string2Chars(secretMessage)
    val encoded: List[Bit] = encode(frenchCode)(secretMessaheChars)
    val quickEncoded: List[Bit] = quickEncode(frenchCode)(secretMessaheChars)
    assert(encoded === quickEncoded)
  }

  test("") {
    new TestTrees {
      val expectedCodeTable: CodeTable = List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)))
      assert(convert(t2) === expectedCodeTable)
    }
  }
}
