namespace Flux.Collections

[<Struct>]
type KeyNotFound<'K> = KeyNotFound of Key: 'K

exception KeyNotFoundException of Msg: string * KeyNotFound: obj KeyNotFound with
    override this.Message = this.Msg

    static member inline throw key =
        KeyNotFoundException ("Key not found in Hamt", KeyNotFound (upcast key))
        |> raise

[<Struct>]
type EntryNotFound = EntryNotFound of Msg: string

exception EntryNotFoundException of EntryNotFound: EntryNotFound with
    override this.Message =
        match this.EntryNotFound with
        | EntryNotFound msg -> msg
       
module internal Key =
    open Flux.Bit
    open System.Collections.Generic

    [<Literal>]
    let HashSize = 32<bit>

    let inline uhash (eqComparer: #IEqualityComparer<_>) key = key |> eqComparer.GetHashCode |> uint32

    let inline equals (eqComparer: #IEqualityComparer<_>) k1 k2 = eqComparer.Equals (k1, k2)

namespace Flux.Collections.Internals.Hamt

open Flux
open Flux.Bit
open Flux.Collections

#if X64
type internal BitmapHolder = Bitmap64
#else
type internal BitmapHolder = Bitmap32
#endif

[<Struct>]
type Prefix = Prefix of bits: uint32 * length: int<bit>

module internal SimplePrefix =
    
    [<Literal>]
    let RootLength = Key.HashSize
    
    let inline fromHash length hash = rshift hash (Key.HashSize - length)
    
    let inline nextLayerLength currentLength =
        if currentLength <= Bitmap.bitIndexBits<BitmapHolder>
            then 0<bit>
            else currentLength - Bitmap.bitIndexBits<BitmapHolder>

module internal Prefix =

    let inline currentLevelPrefixFromHash currentLength hash =
        Prefix (rshift hash (Key.HashSize - currentLength), currentLength)

    let inline fullPrefixFromHash hash =
        currentLevelPrefixFromHash Key.HashSize hash
        
    let inline nextLayerPrefix (Prefix (bits, length)) =
        let shift = min length Bitmap.bitIndexBits<BitmapHolder>
        Prefix (rshift bits shift, length - shift)

    let inline bits (Prefix (bits, _)) = bits

    let inline length (Prefix (_, length)) = length

module internal SimpleBranch =
    
    /// <summary>
    /// Returns the index of the child to which the prefix points to. It's basically the numeric value of the prefix
    /// after with take only the first N bits that represent the current level of the tree.
    /// </summary>
    /// <example>childBitIndex xx...x00101 = 5 (101)</example>
    let inline childBitIndex prefixBits =
        prefixBits &&& Bitmap.bitIndexMask<BitmapHolder> |> asBits
  
module internal Branch =
    
    /// <summary> Returns the index of the child to which the prefix points to </summary>
    /// <example> <code> childBitIndex xx...x00101 = 5 (101) </code> </example>
    /// <seealso cref="Flux.Collections.Internals.Hamt.SimpleBranch.childBitIndex"/>
    let inline childBitIndex prefix =
        (Prefix.bits prefix) &&& Bitmap.bitIndexMask<BitmapHolder> |> asBits

    /// Returns true if the bit to which childBitIndex (index of the child) points to is on
    let inline containsChild indexOfBit bitmap = Bitmap.isBitOn indexOfBit bitmap

    /// <summary>
    /// Returns the index to which childBitIndex points to. The array contains only elements
    /// for which the bitmap has bits on. If the bitmap has only 3 bits on, the children array must have 3 elements.
    /// For a bitmap 0..001000001, only the bits 0 and 6 are on, so the children array would have two items.
    /// </summary>
    /// <example> <code>childArrayIndex 6 0..001000001 = 1 </code> </example>
    /// <remarks>It might be necessary to first ensure that the target bit is on in the bitmap by calling
    /// <see cref="containsChild"/> before using this function</remarks>
    let inline childArrayIndex indexOfBit bitmap =
        bitmap |> Bitmap.bitsLowerThan indexOfBit |> Bitmap.countBitsOn |> int
        
    /// <summary>
    /// Returns the index from low to high significance of the specified bit in the bitmap. Similar to
    /// <see cref="childArrayIndex"/> but instead of passing the index of the bit, the bit itself is passed.
    /// </summary>
    /// <example> <code>childArrayIndex 100000 0..001000001 = 1 </code> </example>
    /// <example> <code>childArrayIndexOfBit (Bitmap.bit index) bitmap = childArrayIndex index bitmap </code> </example>
    /// <remarks>The bit must be enabled on the bitmap for it to work as expected</remarks>
    let inline childArrayIndexOfBit bit bitmap =
        bitmap |> Bitmap.bitsLowerThanBit bit |> Bitmap.countBitsOn |> int
        
    //TODO: Delete this
    /// <summary>
    /// Returns the index of the child in the array represented by <paramref name="branchBitmap"/>
    /// that corresponds to the least significant digit on <paramref name="bitmap"/>. It takes the least significant bit
    /// of <paramref name="bitmap"/> and returns the index that that bit represents if that bit was also enable on
    /// <paramref name="branchBitmap"/>. 
    /// </summary>
    /// <param name="bitmap">The bitmap to which the least significant bit will be taken to evaluate match with <paramref name="branchBitmap"/></param>
    /// <param name="branchBitmap">The bitmap representing the child array</param>
    /// <example>
    /// <br/>Example 1:
    /// <code>firstChildArrayIndex 010100 111111 = 2 (000100)</code>
    /// it would be the third bit because there are two bits enabled in the source bitmap less significant than the
    /// least significant bit of the target bitmap
    /// </example>
    /// <example>
    /// <br/>Example 2:
    /// <code>firstChildArrayIndex 010100 111011 = 2 (000100)</code>
    /// it would be the third bit, even though the bit is not enabled on the source bitmap
    /// </example>
    /// <example>
    /// <br/>Example 3:
    /// <code>firstChildArrayIndex 010000 010001 = 1 (010000)</code> it would be the second bit</example>
    /// <example>
    /// <br/>Example 4:
    /// <code>firstChildArrayIndex 000010 010010 = 0 (000010)</code> it would be the first bit</example>
    let inline childArrayIndexForLeastSignificantBitOfBitmap bitmap branchBitmap =
        let targetBit = Bitmap.getLeastSignificantBitOn bitmap
        let lowerBits = Bitmap.bitsLowerThanBit targetBit branchBitmap
        Bitmap.countBitsOn lowerBits |> int