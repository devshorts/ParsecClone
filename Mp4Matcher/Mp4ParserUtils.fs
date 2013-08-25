namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System
open System.Text

[<AutoOpen>]
module Mp4ParserUtils = 
    let bp = new BinParser(Array.rev)

    let private epochSince1904 = new DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc);

    let date<'a> = bp.byte4 |>> (bp.toUInt32 >> Convert.ToDouble >> epochSince1904.AddMilliseconds)
          
    let stringId<'a> = bp.byte4  |>> Encoding.ASCII.GetString

    let name<'a> (value:string) = bp.matchBytes (Encoding.ASCII.GetBytes value) |>> Encoding.ASCII.GetString

    let atomSize<'a> = bp.uint32

    let skipRemaining (start : uint32) consumed = bp.byteN ((int)start - consumed)
         
    let atom<'a> id =
        attempt (
            atomSize >>= fun size ->
            name id >>= fun name ->
                preturn 
                    {
                        Size = size
                        Name = name
                    }
        )

    let unknown<'a> =
        atomSize >>= fun size ->
        stringId >>= fun name ->
        preturn 
            {
                Size = size
                Name = name
            }

    let versionAndFlags<'a> = 
        bp.byte1 |>> bp.byteToUInt  >>= fun version ->
        bp.byte3 |>> bp.toUInt24    >>= fun flags ->
        preturn 
            {
                Version = version
                Flags = flags
            }

