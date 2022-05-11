    concrete FoodEng of Food = {

      lincat
        Utt, Phrase, Item, Quality = {s : Str} ;
        Kind = {s : Number => Str} ;
  
      lin
        Utterance phrase = {s = phrase.s} ;
        ButUtterance phrase = {s = "excuse me but" ++ phrase.s} ;
        Is item quality = {s = item.s ++ "is" ++ quality.s} ;
        IsQ item quality = {s = "is" ++ item.s ++ quality.s} ;
        This kind = {s = "this" ++ kind.s} ;
        That kind = {s = "that" ++ kind.s} ;
        QKind quality kind = {s = quality.s ++ kind.s} ;
        Wine = {s = "wine"} ;
        Cheese = {s = "cheese"} ;
        Fish = {s = "fish"} ;
        Pasta = {s = "pasta"} ;
        Pie = {s = "pie"} ;
        Cake = {s = "cake"} ;
        Casserole = {s = "casserole"} ;
        Steak = {s = "steak"} ;
        Very quality = {s = "very" ++ quality.s} ;
        Fresh = {s = "fresh"} ;
        Warm = {s = "warm"} ;
        Italian = {s = "Italian"} ;
        Expensive = {s = "expensive"} ;
        Delicious = {s = "delicious"} ;
        Boring = {s = "boring"} ;
        Yummy = {s = "yummy"} ;
        Disgusting = {s = "disgusting"} ;
        Flavorful = {s = "flavorful"} ;
        Cheap = {s = "cheap"} ;
        Affordable = {s = "affordable"} ;
    }