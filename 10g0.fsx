open System

let int2Value (x:int) : int =
    match x with
    | 11 -> 10
    | 12 -> 10
    | 13 -> 10
    | 1 -> 11
    | x -> x

type player (playStyle: int, name: string) =
    let name = name
    let gen = System.Random ()
    let mutable currentHand = []
    member this.playStyle () = playStyle
    member this.showHand () = currentHand
    member this.giveHand () = currentHand <- [int2Value (gen.Next(1, 14));int2Value (gen.Next(1, 14))] @ currentHand
    member this.Hit () = currentHand <- [int2Value (gen.Next(1, 14))] @ currentHand
    member this.Stand () = currentHand <- currentHand
    member this.Sum () =
        let mutable realSum = List.sum currentHand 
        if realSum > 21 then
            for i = 0 to currentHand.Length - 1 do
                if currentHand.[i] = 11 && realSum > 21 then
                    realSum <- realSum - 10
                else
                    realSum <- realSum
            realSum
        else
            realSum
    member this.IsBust () : bool = 
        this.Sum () > 21

let getInput () =
    System.Console.Write ("give input: ")
    let s = System.Console.ReadLine ()
    s 
let initRemovePlayer (playerList : player list) (t : player)=
    let mutable notBustPlayers = []
    let rec removePlayer playerList (t: player) =
        let bustPlayer = t
        match playerList with
            | bustPlayer :: xs -> removePlayer xs t
            | x :: xs -> 
                notBustPlayers <- [x] @ notBustPlayers
                removePlayer xs t
            | _ -> notBustPlayers
    notBustPlayers

type simpleJack (playerList: player list) =
    let dealer = player (1,"dealer")
    let mutable playerListIncDealer = playerList @ [dealer]
    member this.StartGame () = 
        for i = 0 to playerList.Length - 1 do
            playerList.[i].giveHand ()
    member this.OneRound () = 
        for i = 0 to playerListIncDealer.Length - 1 do
            if playerListIncDealer.[i].IsBust () then
                playerListIncDealer <- initRemovePlayer playerListIncDealer playerListIncDealer.[i]
                
            else
                if playerListIncDealer.[i].playStyle () = 0 then
                    let choice = getInput ()
                    if choice = "hit" then
                        playerListIncDealer.[i].Hit ()
                    elif choice = "stand" then
                        playerListIncDealer.[i].Stand ()
                elif playerListIncDealer.[i].playStyle () = 1 then
                    if playerListIncDealer.[i].Sum () > 17 then
                        playerListIncDealer.[i].Stand ()
                    else
                        playerListIncDealer.[i].Hit ()
                else 
                    let gen = System.Random ()
                    let randomChoice = gen.Next(1, 3)
                    if randomChoice = 1 then
                        playerListIncDealer.[i].Stand ()
                    else
                        playerListIncDealer.[i].Hit ()

            


        

     
    
    


let Anders = player ()
Anders.giveHand ()
printfn "%A" (Anders.showHand ())
Anders.Hit ()
printfn "%A" (Anders.showHand ())
printfn "%A" (Anders.Sum ())
printfn "%A" (Anders.IsBust ())