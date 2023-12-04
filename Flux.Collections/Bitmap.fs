namespace Flux

open Bit

module Bitmap =

    let inline underlying< ^T, ^A when ^T: (static member Underlying: 'T -> 'A)> (v: 'T) =
        (^T: (static member Underlying: 'T -> 'A) v)

    let inline private unequal (a: 'B) (b: 'B) = underlying a <> underlying b

    let inline private equal (a: 'B) (b: 'B) = underlying a = underlying b

    let inline lessThanAsNumber< ^T, ^A when ^T: (static member LessThanAsNumber: 'T * 'T -> bool)> (a: 'T) (b: 'T) =
        (^T: (static member LessThanAsNumber: 'T * 'T -> bool) (a, b))

    let inline noBit< ^T when ^T: (static member NoBit: unit -> ^T)> =
        (^T: (static member NoBit: unit -> 'T) ())

    let inline firstBit< ^T when ^T: (static member FirstBit: unit -> ^T)> =
        (^T: (static member FirstBit: unit -> 'T) ())

    let inline allBits< ^T when ^T: (static member FirstBit: unit -> ^T)> =
        (^T: (static member FirstBit: unit -> 'T) ())

    let inline bitIndexBits< ^T when ^T: (static member BitIndexBits: unit -> int<bit>)> =
        (^T: (static member BitIndexBits: unit -> int<bit>) ())

    let inline bitIndexMask< ^T when ^T: (static member BitIndexMask: unit -> uint32)> =
        (^T: (static member BitIndexMask: unit -> uint32) ())

    let inline countBitsOn< ^T when ^T: (static member CountBitsOn: 'T -> int<bit>)> actualBits =
        (^T: (static member CountBitsOn: 'T -> int<bit>) actualBits)

    let inline difference x y = x &&& ~~~y

    let inline union x y = x ||| y

    let inline intersection x y = x &&& y

    /// <summary> Creates a bitmap with only the bit index-th on </summary>
    /// <example>
    /// <code>
    /// bit 3 = ...001000
    /// bit 0 = ...000001
    /// </code>
    /// </example>
    let inline bit index = lshift firstBit index

    let inline isBitOn index bitmap = unequal (bit index &&& bitmap) noBit

    let inline bitsLowerThan index bitmap = (bit index - firstBit) &&& bitmap

    /// <summary>
    /// Creates a bitmap with all the bits on that are less significant than the single bit
    /// </summary>
    /// <param name="bit">A bitmap with exactly one bit on</param>
    /// <param name="bitmap">A bitmap</param>
    /// <returns>A bitmap with all the bits on in <paramref name="bitmap"/> that are less significant than the single bit on in <paramref name="bit"/></returns>
    /// <example><code>bitsLowerThanBit 00100 10110 = 00010</code></example>
    /// <example><code>bitsLowerThanBit 00100 11100 = 00000</code></example>
    /// <remarks>
    /// For performance reasons, no validation is made on <paramref name="bit"/> to ensure only one bit is on.
    /// </remarks>
    let inline bitsLowerThanBit bit bitmap = (bit - firstBit) &&& bitmap

    let inline setBit index bitmap = union bitmap (bit index)

    let inline clearBit index bitmap = difference bitmap (bit index)

    let inline areAllBitsOff bitmap = equal noBit bitmap

    let inline hasAtLeastOneBitOn bitmap = unequal noBit bitmap

    let inline getLeastSignificantBitOn x = x &&& -x

    let inline clearLeastSignificantBit bitmap =
        difference bitmap (getLeastSignificantBitOn bitmap)

    /// <summary>
    /// Like <see cref="clearLeastSignificantBit"/> but also returns the least significant bit
    /// </summary>
    /// <param name="bitmap"></param>
    let inline clearLeastSignificantBitAndReturn bitmap =
        let leastSignificantBit = getLeastSignificantBitOn bitmap
        struct (difference bitmap leastSignificantBit, leastSignificantBit)

[<Struct>]
[<CustomEquality>]
[<NoComparison>]
[<StructuredFormatDisplay("{Display}")>]
type Bitmap32 =
    | Bitmap32 of uint32

    ///All bits off.
    static member inline NoBit() = Bitmap32 (0u)

    ///Only the first (least significant) bit on.
    static member inline FirstBit() = Bitmap32 (1u)

    ///Gives the count of bits that are on.
    static member inline CountBitsOn(Bitmap32 v) = countBitsOn32 v

    ///A bit mask that represent the necessary bits to get an index to any bit in this type.
    ///0x1Fu is (00000000_00000000_00000000_00011111) which is the five bits required to index any bit in a 32 bit bitmap.
    static member inline BitIndexMask() = 0x1Fu

    ///The number of bits needed to represent the index of a bit in this type.
    static member inline BitIndexBits() = 5<bit>

    ///Returns the underlying type used to store this bitmap
    static member inline Underlying(Bitmap32 v) = v

    static member inline (~-)(Bitmap32 value) =
        value |> int |> (~-) |> uint32 |> Bitmap32

    static member inline (-)(Bitmap32 minuend, Bitmap32 subtrahend) = Bitmap32 (minuend - subtrahend)
    static member inline (<<<)(Bitmap32 value, offset) = Bitmap32 (value <<< offset)
    static member inline (>>>)(Bitmap32 value, offset) = Bitmap32 (value >>> offset)
    static member inline (&&&)(Bitmap32 left, Bitmap32 right) = Bitmap32 (left &&& right)
    static member inline (|||)(Bitmap32 left, Bitmap32 right) = Bitmap32 (left ||| right)
    static member inline (~~~)(Bitmap32 single) = Bitmap32 (~~~single)
    static member inline LessThanAsNumber(Bitmap32 a, Bitmap32 b) = a < b
    static member inline op_Equality(Bitmap32 a, Bitmap32 b) = a = b

    override this.ToString() =
        match this with
        | Bitmap32 x -> $"%B{x}"

    member this.Display = this.ToString ()

    interface System.IEquatable<Bitmap32> with
        member this.Equals other = Bitmap32.(=) (this, other)

[<Struct>]
[<CustomEquality>]
[<NoComparison>]
[<StructuredFormatDisplay("{Display}")>]
type Bitmap64 =
    | Bitmap64 of uint64

    static member inline NoBit() = Bitmap64 (0UL)
    static member inline FirstBit() = Bitmap64 (1UL)
    static member inline CountBitsOn(Bitmap64 v) = countBitsOn64 v
    static member inline BitIndexMask() = 0x3Fu
    static member inline BitIndexBits() = 6<bit>
    static member inline Underlying(Bitmap64 v) = v

    static member inline (~-)(Bitmap64 value) =
        value |> int |> (~-) |> uint64 |> Bitmap64

    static member inline (-)(Bitmap64 minuend, Bitmap64 subtrahend) = Bitmap64 (minuend - subtrahend)
    static member inline (<<<)(Bitmap64 value, offset) = Bitmap64 (value <<< offset)
    static member inline (>>>)(Bitmap64 value, offset) = Bitmap64 (value >>> offset)
    static member inline (&&&)(Bitmap64 left, Bitmap64 right) = Bitmap64 (left &&& right)
    static member inline (|||)(Bitmap64 left, Bitmap64 right) = Bitmap64 (left ||| right)
    static member inline (~~~)(Bitmap64 single) = Bitmap64 (~~~single)
    static member inline lessThanAsNumber(Bitmap64 a, Bitmap64 b) = a < b
    static member inline op_Equality(Bitmap64 a, Bitmap64 b) = a = b

    override this.ToString() =
        match this with
        | Bitmap64 x -> $"%B{x}"

    member this.Display = this.ToString ()

    interface System.IEquatable<Bitmap64> with
        member this.Equals other = Bitmap64.(=) (this, other)
