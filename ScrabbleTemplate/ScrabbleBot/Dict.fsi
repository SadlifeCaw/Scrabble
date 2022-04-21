module Dictionary
    type Dictionary = Node of (Map<char, Dictionary>*bool)
    
    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool