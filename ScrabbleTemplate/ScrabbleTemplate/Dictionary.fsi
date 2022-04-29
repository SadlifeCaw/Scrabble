namespace Dictionary

module internal Dictionary =
    type Dictionary = Node of (Map<char, Dictionary>*bool)

    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    
    val step : char -> Dictionary -> (bool * Dictionary) option
    
    val reverse : Dictionary -> (bool * Dictionary) option