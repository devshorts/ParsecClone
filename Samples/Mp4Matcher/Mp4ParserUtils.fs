namespace Mp4Matcher

open ParsecClone.BinaryCombinator
open ParsecClone.CombinatorBase
open System
open System.Text

[<AutoOpen>]
module Mp4ParserUtils = 

    /// <summary>
    /// Creates a network order binary parser
    /// </summary>
    let bp = new BinParser<_>(Array.rev)

    let private epochSince1904 = new DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc);

    let date : VideoParser<_> = bp.byte4 |>> (bp.toUInt32 >> Convert.ToDouble >> epochSince1904.AddMilliseconds)
          
    /// <summary>
    /// Returns a 4 byte sequence as an ascii string
    /// </summary>
    let stringId : VideoParser<_> = bp.byte4  |>> Encoding.ASCII.GetString

    /// <summary>
    /// Matches a string asn ascii byte sequence in the steam
    /// </summary>
    /// <param name="value"></param>
    let name (value:string) : VideoParser<_> = bp.matchBytes (Encoding.ASCII.GetBytes value) |>> Encoding.ASCII.GetString

    let atomSize : VideoParser<_> = bp.uint32

    let skipRemaining (start : uint32) consumed : VideoParser<_> = bp.byteN ((int)start - consumed)
         
    /// <summary>
    /// Parser that sets the user state with the current stream position
    /// </summary>
    let trackStatePosition = 
        statePosition >>= fun pos ->
        getUserState  >>= fun state ->
        setUserState { state with StateStart = pos }

    /// <summary>
    /// Extracts an atom of the target name (if it exists)
    /// and keeps track in the user state what the starting position
    /// of this atom is
    /// </summary>
    /// <param name="id"></param>
    let atom id : VideoParser<_> =
        attempt (
            trackStatePosition >>= fun _ ->
            atomSize >>= fun size ->
            name id >>= fun name -> 
            Console.WriteLine("In " + name)             
            preturn 
                {
                    Size = size
                    Name = name
                }
        )


    /// <summary>
    /// Free atom parser
    /// </summary>
    let free : VideoParser<_> = 
        atom "free"         >>= fun id ->
        skipRemaining id.Size 8  >>= fun _ ->
        preturn id  |>> FREE

    /// <summary>
    /// Optional free atom parser
    /// </summary>
    let freeOpt : VideoParser<_> = opt free
        
   
    /// <summary>
    /// Consumes an atom if its name is a valid 4 letter lowercase ascii sequence    
    /// </summary>
    let unknown : VideoParser<_> = 
        attempt (  
            trackStatePosition >>= fun _ ->     
            atomSize >>= fun size ->
            stringId >>= fun name ->     

            let intValues = name.ToCharArray() |> Array.map (int)  
            let nonAscii = Array.exists(fun i -> i < 97 || i > 122) intValues 

            if nonAscii then 
                pzero 
            else                
                skipRemaining size 8 >>. 
                preturn 
                    {
                        Size = size
                        Name = name
                    }           
        )

        
    /// <summary>
    /// Runs the parser while the user state
    /// stream pos - start < size
    /// </summary>
    /// <param name="start"></param>
    /// <param name="size"></param>
    /// <param name="parser"></param>
    let satisfyAtomSize start size parser = 
        satisfyUserState (
            fun (s:VideoState) -> 
                let consumed = s.StateStart - (int64)start
                consumed < (int64)size
            ) parser 
        
    /// <summary>
    /// runs the parser many times until the 
    /// target amount is consumed (making sure the atom has been fully consumed)    
    /// </summary>
    /// <param name="name"></param>
    /// <param name="getParser"></param>
    let fullConsume name getParser = 
        atom name >>= fun id ->
        getUserState >>= fun state ->
        let parser = getParser id
        let consumeTillSize = satisfyAtomSize state.StateStart id.Size parser
        many consumeTillSize

    
    let versionAndFlags : VideoParser<_> = 
        bp.byte1 |>> bp.byteToUInt  >>= fun version ->
        bp.byte3 |>> bp.toUInt24    >>= fun flags ->
        preturn 
            {
                Version = version
                Flags = flags
            }

