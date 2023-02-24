[<CompiledName("InternalUse")>]
module internal Flux.Collections.KeyEqualityComparison

open System
open System.Collections.Generic

type Marker =
    interface
    end

let thisModule = lazy typeof<Marker>.DeclaringType

let nonStructuralEqualityComparerForIEquatableMethodInfo =
    lazy thisModule.Value.GetMethod("nonStructuralEqualityComparerForIEquatable")

let inline nonStructuralEqualityComparer<'T when 'T: equality> =
    { new IEqualityComparer<'T> with
        member _.GetHashCode x = NonStructuralComparison.hash x

        member _.Equals(x, y) =
            match box x, box y with
            | null, null -> true
            | _, null
            | null, _ -> false
            | _ -> x.Equals y (* Object Equals*) }

let inline nonStructuralEqualityComparerForIEquatable<'T when 'T: equality and 'T :> IEquatable<'T>> =
    { new IEqualityComparer<'T> with
        member _.GetHashCode x = NonStructuralComparison.hash x

        member _.Equals(x, y) =
            match box x, box y with
            | null, null -> true
            | _, null
            | null, _ -> false
            | _ -> x.Equals y (* IEquatable Equals*) }

let selectStructuralEqualityComparer<'T when 'T: equality> =
    match typeof<'T> with
    //Primitive types  is not structural and is a known type relatively common as a key in a Dictionary it makes no sense
    //to try to create an structural equality comparer
    | ty when typeof<int32>.Equals ty -> HashIdentity.NonStructural<int32> :?> IEqualityComparer<'T>
    | ty when typeof<string>.Equals ty -> HashIdentity.NonStructural<string> :?> IEqualityComparer<'T>
    | ty when typeof<Guid>.Equals ty -> HashIdentity.NonStructural<Guid> :?> IEqualityComparer<'T>
    | ty when typeof<uint32>.Equals ty -> HashIdentity.NonStructural<uint32> :?> IEqualityComparer<'T>
    | ty when typeof<int64>.Equals ty -> HashIdentity.NonStructural<int64> :?> IEqualityComparer<'T>
    | ty when typeof<uint64>.Equals ty -> HashIdentity.NonStructural<uint64> :?> IEqualityComparer<'T>
    | ty when typeof<bool>.Equals ty -> HashIdentity.NonStructural<bool> :?> IEqualityComparer<'T>
    | ty when typeof<byte>.Equals ty -> HashIdentity.NonStructural<byte> :?> IEqualityComparer<'T>
    | ty when typeof<char>.Equals ty -> HashIdentity.NonStructural<char> :?> IEqualityComparer<'T>
    | ty when typeof<sbyte>.Equals ty -> HashIdentity.NonStructural<sbyte> :?> IEqualityComparer<'T>
    | ty when typeof<int16>.Equals ty -> HashIdentity.NonStructural<int16> :?> IEqualityComparer<'T>
    | ty when typeof<nativeint>.Equals ty -> HashIdentity.NonStructural<nativeint> :?> IEqualityComparer<'T>
    | ty when typeof<unativeint>.Equals ty -> HashIdentity.NonStructural<unativeint> :?> IEqualityComparer<'T>
    | ty when typeof<uint16>.Equals ty -> HashIdentity.NonStructural<uint16> :?> IEqualityComparer<'T>
    | ty when typeof<float>.Equals ty -> HashIdentity.NonStructural<float> :?> IEqualityComparer<'T>
    | ty when typeof<float32>.Equals ty -> HashIdentity.NonStructural<float32> :?> IEqualityComparer<'T>
    | ty when typeof<decimal>.Equals ty -> HashIdentity.NonStructural<decimal> :?> IEqualityComparer<'T>
    | _ -> HashIdentity.Structural<'T>

let selectNonStructuralEqualityComparer<'T when 'T: equality> : IEqualityComparer<'T> =
    match typeof<'T> with
    | ty when typeof<int32>.Equals ty -> HashIdentity.NonStructural<int32> :?> IEqualityComparer<'T>
    | ty when typeof<string>.Equals ty -> HashIdentity.NonStructural<string> :?> IEqualityComparer<'T>
    | ty when typeof<Guid>.Equals ty -> HashIdentity.NonStructural<Guid> :?> IEqualityComparer<'T>
    | ty when typeof<uint32>.Equals ty -> HashIdentity.NonStructural<uint32> :?> IEqualityComparer<'T>
    | ty when typeof<int64>.Equals ty -> HashIdentity.NonStructural<int64> :?> IEqualityComparer<'T>
    | ty when typeof<uint64>.Equals ty -> HashIdentity.NonStructural<uint64> :?> IEqualityComparer<'T>
    | ty when typeof<bool>.Equals ty -> HashIdentity.NonStructural<bool> :?> IEqualityComparer<'T>
    | ty when typeof<byte>.Equals ty -> HashIdentity.NonStructural<byte> :?> IEqualityComparer<'T>
    | ty when typeof<char>.Equals ty -> HashIdentity.NonStructural<char> :?> IEqualityComparer<'T>
    | ty when typeof<sbyte>.Equals ty -> HashIdentity.NonStructural<sbyte> :?> IEqualityComparer<'T>
    | ty when typeof<int16>.Equals ty -> HashIdentity.NonStructural<int16> :?> IEqualityComparer<'T>
    | ty when typeof<nativeint>.Equals ty -> HashIdentity.NonStructural<nativeint> :?> IEqualityComparer<'T>
    | ty when typeof<unativeint>.Equals ty -> HashIdentity.NonStructural<unativeint> :?> IEqualityComparer<'T>
    | ty when typeof<uint16>.Equals ty -> HashIdentity.NonStructural<uint16> :?> IEqualityComparer<'T>
    | ty when typeof<float>.Equals ty -> HashIdentity.NonStructural<float> :?> IEqualityComparer<'T>
    | ty when typeof<float32>.Equals ty -> HashIdentity.NonStructural<float32> :?> IEqualityComparer<'T>
    | ty when typeof<decimal>.Equals ty -> HashIdentity.NonStructural<decimal> :?> IEqualityComparer<'T>
    //For IEquatable types we create a IEqualityComparer that uses the interface Equals
    | ty when ty.IsAssignableTo typeof<IEquatable<'T>> ->
        let method =
            nonStructuralEqualityComparerForIEquatableMethodInfo.Value.MakeGenericMethod typeof<'T>

        method.Invoke(null, [||]) :?> IEqualityComparer<'T>
    | _ -> nonStructuralEqualityComparer<'T> //HashIdentity.NonStructural<'T> would force static member (=) constraint on 'T
