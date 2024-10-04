import java.io.BufferedReader
import java.io.FileReader

fun change(amount: Long): Map<Int, Long> {
    require(amount >= 0) { "Amount cannot be negative" }

    val counts = mutableMapOf<Int, Long>()
    var remaining = amount
    for (denomination in listOf(25, 10, 5, 1)) {
        counts[denomination] = remaining / denomination
        remaining %= denomination
    }
    return counts
}

fun firstThenLowerCase(strings: List<String>, condition: (String) -> Boolean): String? {
    return strings.firstOrNull(condition)?.lowercase()
}

class SayObject private constructor(private val words: List<String>) {
    val phrase: String
        get() = words.joinToString(" ")
    fun and(word: String): SayObject = SayObject(words + word)
    companion object {
        operator fun invoke(initial: String = ""): SayObject = SayObject(listOf(initial))
    }
}
fun say(initial: String = ""): SayObject = SayObject(initial)

fun meaningfulLineCount(fileName: String): Long {
    var count = 0L
    FileReader(fileName).use { fileReader ->
        BufferedReader(fileReader).use { bufferedReader ->
            bufferedReader.forEachLine { line ->
                var trimmedLines = line.trim()
                if (!trimmedLines.isEmpty()) {
                    if (!(trimmedLines[0] == '#')) {
                        count += 1L
                    }
                }
            }
        }
        return count
    }
}

data class Quaternion(val a: Double, val b: Double, val c: Double, val d: Double) {
    operator fun plus(other: Quaternion) =
        Quaternion(a + other.a, b + other.b, c + other.c, d + other.d)

    operator fun times(other: Quaternion) =
        Quaternion(
            a * other.a - b * other.b - c * other.c - d * other.d,
            a * other.b + b * other.a + c * other.d - d * other.c,
            a * other.c - b * other.d + c * other.a + d * other.b,
            a * other.d + b * other.c - c * other.b + d * other.a
        )

    fun coefficients() = listOf(a, b, c, d)
    fun conjugate() = Quaternion(a, -b, -c, -d)

    override fun toString(): String {
        val terms =
            listOf(a to "", b to "i", c to "j", d to "k")
                .filter { it.first != 0.0 }
                .map {
                    when {
                        it.first == 1.0 && it.second.isNotEmpty() -> it.second
                        it.first == -1.0 && it.second.isNotEmpty() -> "-${it.second}"
                        else -> "${it.first}${it.second}"
                    }
                }
        return when {
            terms.isEmpty() -> "0"
            terms.size == 1 -> terms[0]
            else -> terms.joinToString("+").replace("+-", "-")
        }
    }

    companion object {
        val ZERO = Quaternion(0.0, 0.0, 0.0, 0.0)
        val I = Quaternion(0.0, 1.0, 0.0, 0.0)
        val J = Quaternion(0.0, 0.0, 1.0, 0.0)
        val K = Quaternion(0.0, 0.0, 0.0, 1.0)
    }
}

sealed interface BinarySearchTree {

    fun size(): Int
    fun contains(letter: String): Boolean
    fun insert(letter: String): BinarySearchTree

    companion object {
        val Empty: BinarySearchTree = EmptyTree
    }

    private object EmptyTree : BinarySearchTree {
        override fun size(): Int = 0
        override fun contains(letter: String): Boolean = false
        override fun insert(letter: String): BinarySearchTree = Node(letter, Empty, Empty)
        override fun toString(): String = "()"
    }

    private data class Node(
        val letter: String,
        val left: BinarySearchTree,
        val right: BinarySearchTree
    ) : BinarySearchTree {
        override fun size(): Int = 1 + left.size() + right.size()

        override fun contains(letter: String): Boolean =
            when {
                this.letter == letter -> true
                letter < this.letter -> left.contains(letter)
                else -> right.contains(letter)
            }

        override fun insert(letter: String): BinarySearchTree =
            when {
                letter < this.letter -> Node(this.letter, left.insert(letter), right)
                letter > this.letter -> Node(this.letter, left, right.insert(letter))
                else -> this
            }

        override fun toString(): String =
            when {
                left == Empty && right == Empty -> "($letter)"
                left == Empty -> "($letter$right)"
                right == Empty -> "($left$letter)"
                else -> "($left$letter$right)"
            }
    }
}
