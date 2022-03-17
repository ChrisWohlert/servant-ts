interface User { 
  userId : number
  userFirstName : string
  userLastName : string
}
interface Team { 
  players : Array<Player>
  teamName : string
}
interface Player { 
  playerName : string
  playerRank : number
}
