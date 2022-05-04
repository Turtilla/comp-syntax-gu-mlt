    concrete FoodSwe of Food = {
  
      lincat
        Utt, Phrase, Item, Kind, Quality = {s : Str} ;
  
      lin
        Utterance phrase = {s = phrase.s} ;
        ButUtterance phrase = {s = "ursäkta men" ++ phrase.s} ;
        Is item quality = {s = item.s ++ "är" ++ quality.s} ;
        IsQ item quality = {s = "är" ++ item.s ++ quality.s} ;
        This kind = {s = "den här" ++ kind.s} ;
        That kind = {s = "den där" ++ kind.s} ;
        QKind quality kind = {s = quality.s ++ kind.s} ;
        Wine = {s = "vinet"} ;
        Cheese = {s = "osten"} ;
        Fish = {s = "fisken"} ;
        Pasta = {s = "pastan"} ;
        Pie = {s = "pajen"} ;
        Cake = {s = "tårtan"} ;
        Casserole = {s = "grytan"} ;
        Steak = {s = "biffen"} ;
        Very quality = {s = "mycket" ++ quality.s} ;
        Fresh = {s = "färsk"} ;
        Warm = {s = "varm"} ;
        Italian = {s = "italiensk"} ;
        Expensive = {s = "dyr"} ;
        Delicious = {s = "god"} ;
        Boring = {s = "tråkig"} ;
        Yummy = {s = "god"} ;
        Disgusting = {s = "äcklig"} ;
        Flavorful = {s = "smakrik"} ;
        Cheap = {s = "billig"} ;
        Affordable = {s = "prisvärd"} ;
    }