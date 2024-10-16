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

export function firstThenApply<T, U>(
  items: T[],
  predicate: (item: T) => boolean,
  consumer: (item: T) => U
): U | undefined {
  const foundItem = items.find(predicate);
  return foundItem !== undefined ? consumer(foundItem) : undefined;
}

export function* powersGenerator(base: bigint): Generator<bigint> {
  for (let power = 1n; ; power *= base) {
    yield power
  }
}

// Write your line count function here
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

// Write your shape type and associated functions here
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
  return 0 // fix this
}

export function volume(shape: Shape): number {
  return 0 // fix this
}

// Write your binary search tree implementation here
export interface BinarySearchTree<T> {
  size(): number
  isGeneratorFunction(value: T): BinarySearchTree<T>
  contains(value: T): boolean
  inorder(): Iterable<T>
}
