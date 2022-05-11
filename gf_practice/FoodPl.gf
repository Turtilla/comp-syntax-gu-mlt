    concrete FoodPl of Food = open StringOper in {
  
      lincat
        Utt, Phrase, Item, Kind, Quality = {s : Str} ;
  
      lin
        Utterance phrase = {s = phrase.s} ;
        ButUtterance phrase = {s = "przepraszam ale" ++ phrase.s} ;
        Is item quality = {s = item.s ++ "jest" ++ quality.s} ;
        IsQ item quality = {s = "czy" ++ item.s ++ "jest" ++ quality.s} ;
        This kind = {s = "ten" ++ kind.s} ;
        That kind = {s = "tamten" ++ kind.s} ;
        QKind quality kind = {s = quality.s ++ kind.s} ;
        Wine = {s = "wino"} ;
        Cheese = {s = "ser"} ;
        Fish = {s = "ryba"} ;
        Pasta = {s = "makaron"} ;
        Pie = {s = "paj"} ;
        Cake = {s = "ciasto"} ;
        Casserole = {s = "zapiekanka"} ;
        Steak = {s = "stek"} ;
        Very quality = {s = "bardzo" ++ quality.s} ;
        Fresh = {s = "świeży"} ;
        Warm = {s = "ciepły"} ;
        Italian = {s = "włoski"} ;
        Expensive = {s = "drogi"} ;
        Delicious = {s = "pyszny"} ;
        Boring = {s = "nudny"} ;
        Yummy = {s = "smaczny"} ;
        Disgusting = {s = "paskudny"} ;
        Flavorful = {s = "smakowity"} ;
        Cheap = {s = "tani"} ;
        Affordable = {s = "przystępny"} ;
    }