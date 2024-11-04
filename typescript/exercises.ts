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
  length: number
  depth: number
}

export type Shape = Sphere | Box

export function surfaceArea(shape: Shape): number {
  switch (shape.kind) {
    case "Box":
      return 2 * (shape.width * shape.length + shape.width * shape.depth + shape.length * shape.depth);
    case "Sphere":
      return 4 * Math.PI * Math.pow(shape.radius, 2);
  }
}

export function volume(shape: Shape): number {
  switch (shape.kind) {
    case "Box":
      return shape.width * shape.length * shape.depth;
    case "Sphere":
      return (4 / 3) * Math.PI * Math.pow(shape.radius, 3);
  }
}

export function shapeToString(shape: Shape): string {
  switch (shape.kind) {
    case "Box":
      return `Box(width=${shape.width}, length=${shape.length}, depth=${shape.depth})`;
    case "Sphere":
      return `Sphere(radius=${shape.radius})`;
  }
}

export class Empty<T> {
  readonly isEmpty = true;

  private static _instance: Empty<any> = new Empty();

  constructor() {
    if (Empty._instance) {
      return Empty._instance;
    }
    Empty._instance = this;
  }

  insert(letter: T): Node<T> {
    return new Node(letter, new Empty<T>(), new Empty<T>());
  }

  contains(letter: T): boolean {
    return false;
  }

  size(): number {
    return 0;
  }

  *inorder(): Generator<T> {}

  toString(): string {
    return "()"; 
  }
}

export class Node<T> {
  readonly isEmpty = false;

  constructor(
    public readonly letter: T,
    public readonly left: BinarySearchTree<T>,
    public readonly right: BinarySearchTree<T>
  ) {}

  insert(letter: T): Node<T> {
    if (letter < this.letter) {
      return new Node(this.letter, this.left.insert(letter), this.right);
    } else if (letter > this.letter) {
      return new Node(this.letter, this.left, this.right.insert(letter));
    }
    return this; 
  }

  contains(letter: T): boolean {
    if (letter === this.letter) {
      return true;
    } else if (letter < this.letter) {
      return this.left.contains(letter);
    } else {
      return this.right.contains(letter);
    }
  }

  size(): number {
    return 1 + this.left.size() + this.right.size();
  }

  *inorder(): Generator<T> {
    yield* this.left.inorder();
    yield this.letter;
    yield* this.right.inorder();
  }

  toString(): string {
    const leftStr = this.left.isEmpty ? "" : this.left.toString();
    const rightStr = this.right.isEmpty ? "" : this.right.toString();
    return `(${leftStr}${this.letter}${rightStr})`;
  }
}

export type BinarySearchTree<T> = Node<T> | Empty<T>;

export const emptyTree = <T>(): BinarySearchTree<T> => new Empty<T>();
