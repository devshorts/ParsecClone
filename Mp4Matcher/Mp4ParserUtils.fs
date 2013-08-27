namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System
open System.Text

[<AutoOpen>]
module Mp4ParserUtils = 
    let bp = new BinParser<_>(Array.rev)

    let private epochSince1904 = new DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc);

    let date : VideoParser<_> = bp.byte4 |>> (bp.toUInt32 >> Convert.ToDouble >> epochSince1904.AddMilliseconds)
          
    let stringId : VideoParser<_> = bp.byte4  |>> Encoding.ASCII.GetString

    let name (value:string) : VideoParser<_> = bp.matchBytes (Encoding.ASCII.GetBytes value) |>> Encoding.ASCII.GetString

    let atomSize : VideoParser<_> = bp.uint32

    let skipRemaining (start : uint32) consumed : VideoParser<_> = bp.byteN ((int)start - consumed)
         
    let atom id : VideoParser<_> =
        attempt (
            atomSize >>= fun size ->
            name id >>= fun name ->
            //Console.WriteLine ("in " + name)
            preturn 
                {
                    Size = size
                    Name = name
                }
        )

    let unknown : VideoParser<_> =
        atomSize >>= fun size ->
        stringId >>= fun name ->
        preturn 
            {
                Size = size
                Name = name
            }

    let versionAndFlags : VideoParser<_> = 
        bp.byte1 |>> bp.byteToUInt  >>= fun version ->
        bp.byte3 |>> bp.toUInt24    >>= fun flags ->
        preturn 
            {
                Version = version
                Flags = flags
            }

