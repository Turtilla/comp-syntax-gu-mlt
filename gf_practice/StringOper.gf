resource StringOper = {

    param Number = Sg | Pl ;
    param Gender = Masc | Fem | Neut ;
    param Case = Nom | Gen | Dat | Acc | Ins | Loc | Voc ;
      oper
        mkNEng : Str -> Str -> {s : Number => Str} = \sg,pl -> {
        s = table {Sg => sg ; Pl => pl}
        } ;
        smartNEng : Str -> {s : Number => Str} = \noun -> case noun of {
            x + ("ay"|"ey"|"oy"|"uy") => mkNEng noun (noun + "s") ;
            x + "y" => mkNEng noun (x + "ies") ;
            x + ("ch"|"sh"|"s"|"o") => mkNEng noun (noun + "es") ;
            x => mkNEng noun (noun + "s")
        } ;

        mkNSwe : Str -> Str -> {s : Number => Str} = \sg,pl -> {
        s = table {Sg => sg ; Pl => pl}
        } ;
        smartNSwe : Str -> {s : Number => Str} = \noun -> case noun of {
            x + "an" => mkNSwe noun (x + "orna") ;
            x + "en" => mkNSwe noun (x + "arna") ;
            x + "et" => mkNSwe noun (x + "en") ;
            x => mkNSwe noun (x + "erna")
        } ;

        Noun : Type = {s : Number => Case => Str ; g : Gender} ;

        mkNPl : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Gender -> Noun 
        = \sgnom, sggen, sgdat, sgacc, sgins, sgloc, sgvoc, plnom, plgen, pldat, placc, plins, plloc, plvoc, g -> {
        s = table {
            Sg => table {
                Nom => sgnom ;
                Gen => sggen ;
                Dat => sgdat ;
                Acc => sgacc ;
                Ins => sgins ;
                Loc => sgloc ;
                Voc => sgvoc
            } ;
            Pl => table {
                Nom => plnom ;
                Gen => plgen ;
                Dat => pldat ;
                Acc => placc ;
                Ins => plins ;
                Loc => plloc ;
                Voc => plvoc 
            }
            } ;
            g = g
        } ;

        smartNPl : Str -> Gender -> Noun = \noun,g -> case noun of {
            x + "o" => mkNPl noun (x + "a") (x + "u") noun (x + "iem") (x + "u") noun (x + "a") x (x + "om") (x + "a") (x + "ami") (x + "ach") (x + "a") g ;
            x + "a" => mkNPl noun (x + "y") (x + "ie") (x + "ę") (x + "ą") (x + "ie") (x + "o") (x + "y") x (x + "om") (x + "y") (x + "ami") (x + "ach") (x + "y") g ;
            x + _ => mkNPl noun (noun + "a") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") (noun + "y") (noun + "ów") (noun + "om") (noun + "y") (noun + "ami") (noun + "ach") (noun + "y") g      
        } ;

        SS : Type = {s : Str} ;
        ss : Str -> SS = \x -> {s = x} ;
        cc : SS -> SS -> SS = \x,y -> ss (x.s ++ y.s) ;
        prefix : Str -> SS -> SS = \p,x -> ss (p ++ x.s) ;
        infix : SS -> Str -> SS -> SS = \x,p,y -> ss (x.s ++ p ++ y.s) ;
        leave : SS -> SS = \x -> ss (x.s) ;
    }