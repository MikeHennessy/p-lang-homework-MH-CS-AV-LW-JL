import Foundation

struct NegativeAmountError: Error {}
struct NoSuchFileError: Error {}

func change(_ amount: Int) -> Result<[Int: Int], NegativeAmountError> {
    if amount < 0 {
        return .failure(NegativeAmountError())
    }
    var (counts, remaining) = ([Int: Int](), amount)
    for denomination in [25, 10, 5, 1] {
        (counts[denomination], remaining) =
            remaining.quotientAndRemainder(dividingBy: denomination)
    }
    return .success(counts)
}

func firstThenLowerCase(of words: [String], satisfying predicate: (String) -> Bool) -> String? {
    return words.first(where: predicate)?.lowercased()
}

struct Say {
    private var words: [String]

    init(_ word: String = "") {
        self.words = [word]
    }

    func and(_ word: String) -> Say {
        var newSay = self
        newSay.words.append(word)
        return newSay
    }

    var phrase: String {
        return words.joined(separator: " ")
    }
}

func say(_ word: String = "") -> Say {
    return Say(word)
}

enum FileError: Error {
    case fileNotFound
    case failedToRead
}

func meaningfulLineCount(_ filename: String) async -> Result<Int, Error> {
    do {
        let fileURL = URL(fileURLWithPath: filename)
        guard FileManager.default.fileExists(atPath: filename) else {
            return .failure(FileError.fileNotFound)
        }
        var lineCount = 0
        for try await line in fileURL.lines {
            let trimmedLine = line.trimmingCharacters(in: .whitespaces)

            if !trimmedLine.isEmpty && !trimmedLine.hasPrefix("#") {
                lineCount += 1
            }
        }
        return .success(lineCount)
    } catch {
        return .failure(FileError.failedToRead)
    }
}

struct Quaternion: CustomStringConvertible {
    let a, b, c, d: Double

    init(a: Double? = 0, b: Double? = 0, c: Double? = 0, d: Double? = 0) {
        self.a = a ?? 0
        self.b = b ?? 0
        self.c = c ?? 0
        self.d = d ?? 0
    }

    var coefficients: [Double] {
        return [a, b, c, d]
    }

    var conjugate: Quaternion {
        return Quaternion(a: a, b: -b, c: -c, d: -d)
    }

    var description: String {
        var components: [String] = []
        if a != 0 {
            components.append("\(a)")
        }
        if b != 0 {
            if b == 1 {
                components.append("+i")
            } else if b == -1 {
                components.append("-i")
            } else if b > 0 {
                components.append("+\(b)i")
            } else {
                components.append("\(b)i")
            }
        }
        if c != 0 {
            if c == 1 {
                components.append("+j")
            } else if c == -1 {
                components.append("-j")
            } else if c > 0 {
                components.append("+\(c)j")
            } else {
                components.append("\(c)j")
            }
        }
        if d != 0 {
            if d == 1 {
                components.append("+k")
            } else if d == -1 {
                components.append("-k")
            } else if d > 0 {
                components.append("+\(d)k")
            } else {
                components.append("\(d)k")
            }
        }

        if components.isEmpty {
            return "0"
        }

        var string_desc = components.joined()
        if string_desc.first == "+" {
            string_desc.removeFirst()
        }

        return string_desc
    }

    static func == (lhs: Quaternion, rhs: Quaternion) -> Bool {
        return (lhs.a == rhs.a && lhs.b == rhs.b && lhs.c == rhs.c && lhs.d == rhs.d)
    }

    static var ZERO: Quaternion {
        return Quaternion(a: 0, b: 0, c: 0, d: 0)
    }

    static var I: Quaternion {
        return Quaternion(a: 0, b: 1, c: 0, d: 0)
    }

    static var J: Quaternion {
        return Quaternion(a: 0, b: 0, c: 1, d: 0)
    }

    static var K: Quaternion {
        return Quaternion(a: 0, b: 0, c: 0, d: 1)
    }
}

func + (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
    return Quaternion(a: lhs.a + rhs.a, b: lhs.b + rhs.b, c: lhs.c + rhs.c, d: lhs.d + rhs.d)
}

func * (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
    return Quaternion(
        a: (lhs.a * rhs.a) - (lhs.b * rhs.b) - (lhs.c * rhs.c) - (lhs.d * rhs.d),
        b: (lhs.a * rhs.b) + (lhs.b * rhs.a) + (lhs.c * rhs.d) - (lhs.d * rhs.c),
        c: (lhs.a * rhs.c) - (lhs.b * rhs.d) + (lhs.c * rhs.a) + (lhs.d * rhs.b),
        d: (lhs.a * rhs.d) + (lhs.b * rhs.c) - (lhs.c * rhs.b) + (lhs.d * rhs.a)
    )
}

enum BinarySearchTree: CustomStringConvertible {
    case empty
    indirect case node(BinarySearchTree, String, BinarySearchTree)

    var size: Int {
        switch self {
        case .empty:
            return 0
        case let .node(left, _, right):
            return left.size + 1 + right.size
        }
    }

    var description: String {
        switch self {
        case .empty:
            return "()"
        case let .node(left, letter, right):
            let leftDesc: String = left.description == "()" ? "" : left.description
            let rightDesc: String = right.description == "()" ? "" : right.description
            if leftDesc.isEmpty && rightDesc.isEmpty {
                return "(\(letter))"
            } else if rightDesc.isEmpty {
                return "(\(leftDesc)\(letter))"
            } else if leftDesc.isEmpty {
                return "(\(letter)\(rightDesc))"
            } else {
                return "(\(leftDesc)\(letter)\(rightDesc))"
            }
        }
    }

    func insert(_ newletter: String) -> BinarySearchTree {
        switch self {
        case .empty:
            return .node(.empty, newletter, .empty)
        case let .node(left, letter, right):
            if newletter < letter {
                return .node(left.insert(newletter), letter, right)
            } else if newletter > letter {
                return .node(left, letter, right.insert(newletter))
            } else {
                return self
            }
        }
    }

    func contains(_ searchletter: String) -> Bool {
        switch self {
        case .empty:
            return false
        case let .node(left, letter, right):
            if searchletter == letter {
                return true
            } else if searchletter < letter {
                return left.contains(searchletter)
            } else {
                return right.contains(searchletter)
            }
        }
    }
}
