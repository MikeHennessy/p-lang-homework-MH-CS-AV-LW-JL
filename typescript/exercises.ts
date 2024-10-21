import { open } from "node:fs/promises"

export function change(amount: bigint): Map<bigint, bigint> {
  if (amount < 0) {
    throw new RangeError("Amount cannot be negative")
  }
  let counts: Map<bigint, bigint> = new Map()
  let remaining = amount
  for (const denomination of [25n, 10n, 5n, 1n]) {
    counts.set(denomination, remaining / denomination)
    remaining %= denomination
  }
  return counts
}

export function firstThenApply<InputType, OutputType>(
  items: InputType[],
  predicate: (item: InputType) => boolean,
  consumer: (item: InputType) => OutputType
): OutputType | undefined {
  const foundItem = items.find(predicate);
  return foundItem !== undefined ? consumer(foundItem) : undefined;
}

export function* powersGenerator(base: bigint): Generator<bigint> {
  for (let power = 1n; ; power *= base) {
    yield power
  }
}

export async function meaningfulLineCount(filename: string): Promise<number> {
  let count = 0;
  const file = await open(filename, "r")
  for await (const line of file.readLines()) {
    const trimmed = line.trim()
    if (trimmed && !trimmed.startsWith("#")) {
      count++
    }
  }
  return count
}

interface Sphere {
  kind: "Sphere"
  radius: number
}

interface Box {
  kind: "Box"
  width: number
  height: number
  depth: number
}

export type Shape = Sphere | Box

export function surfaceArea(shape: Shape): number {
  switch (shape.kind) {
    case "Box":
      return 2 * (shape.width * shape.height + shape.width * shape.depth + shape.height * shape.depth);
    case "Sphere":
      return 4 * Math.PI * Math.pow(shape.radius, 2);
  }
}

export function volume(shape: Shape): number {
  switch (shape.kind) {
    case "Box":
      return shape.width * shape.height * shape.depth;
    case "Sphere":
      return (4 / 3) * Math.PI * Math.pow(shape.radius, 3);
  }
}

export function shapeToString(shape: Shape): string {
  switch (shape.kind) {
    case "Box":
      return `Box(width=${shape.width}, length=${shape.height}, depth=${shape.depth})`;
    case "Sphere":
      return `Sphere(radius=${shape.radius})`;
  }
}

// singleton?

interface Comparable<T> {
  compareTo(other: T): number;
}
interface BinarySearchTree<T extends Comparable<T>> {
  insert(value: T): BinarySearchTree<T>;
  contains(value: T): boolean;
  size(): number;
  inorder(): IterableIterator<T>;
}
class Empty<T extends Comparable<T>> implements BinarySearchTree<T> {
  static instance = new Empty<any>();

  insert(value: T): BinarySearchTree<T> {
    return new Node(value, Empty.instance, Empty.instance);
  }

  contains(_: T): boolean { return false; }

  size(): number { return 0; }

  *inorder(): IterableIterator<T> {}

  toString(): string { return "()"; }
}

class Node<T extends Comparable<T>> implements BinarySearchTree<T> {
  constructor(
    private readonly value: T,
    private readonly left: BinarySearchTree<T>,
    private readonly right: BinarySearchTree<T>
  ) {}

  insert(value: T): BinarySearchTree<T> {
    if (value.compareTo(this.value) < 0) {
      return new Node(this.value, this.left.insert(value), this.right);
    } else if (value.compareTo(this.value) > 0) {
      return new Node(this.value, this.left, this.right.insert(value));
    }
    return this; 
  }

  contains(value: T): boolean {
    if (value.compareTo(this.value) < 0) {
      return this.left.contains(value);
    } else if (value.compareTo(this.value) > 0) {
      return this.right.contains(value);
    }
    return true;
  }

  size(): number {
    return 1 + this.left.size() + this.right.size();
  }

  *inorder(): IterableIterator<T> {
    yield* this.left.inorder();
    yield this.value;
    yield* this.right.inorder();
  }

  toString(): string {
    return `(${this.left}${this.value}${this.right})`;
  }
}

class ComparableString implements Comparable<ComparableString> {
  constructor(private readonly str: string) {}

  compareTo(other: ComparableString): number {
    return this.str.localeCompare(other.str);
  }

  toString(): string {
    return this.str;
  }
}
