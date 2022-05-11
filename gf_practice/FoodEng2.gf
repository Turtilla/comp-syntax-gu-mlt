concrete FoodEng2 of Food = open StringOper in {
  
      lincat
        Utt, Item, Kind, Quality = SS ;
  
      lin
        Utterance phrase = leave phrase ;
        ButUtterance phrase = prefix "excuse me but" phrase ;
        Is item quality = infix item "is" quality ;
        IsQ item quality = cc (prefix "is" item) quality ;
        This k = prefix "this" k ;
        That k = prefix "that" k ;
        QKind k q = cc k q ;
        Wine = ss "wine" ;
        Cheese = ss "cheese" ;
        Fish = ss "fish" ;
        Pasta = ss "pasta" ;
        Pie = ss "pie" ;
        Cake = ss "cake" ;
        Casserole = ss "casserole" ;
        Steak = ss "steak" ;
        Very = prefix "very" ;
        Fresh = ss "fresh" ;
        Warm = ss "warm" ;
        Italian = ss "Italian" ;
        Expensive = ss "expensive" ;
        Delicious = ss "delicious" ;
        Boring = ss "boring" ;
        Yummy = ss "yummy" ;
        Disgusting = ss "disgusting" ;
        Flavorful = ss "flavorful" ;
        Cheap = ss "cheap" ;
        Affordable = ss "affordable" ;
    }