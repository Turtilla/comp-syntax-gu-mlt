resource MicroResTest = open Prelude in {

param
  VForm = Inf | Pres ;
  Number = Sg | Pl ;
  Case = Nom | Gen | Dat | Acc | Ins | Loc | Voc ;
  Gender = Masc | Fem | Neut ;
oper
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
            x + _ => mkNPl noun (noun + "u") (noun + "owi") noun (noun + "em") (noun + "u") (noun + "u") (noun + "y") (noun + "ów") (noun + "om") (noun + "y") (noun + "ami") (noun + "ach") (noun + "y") g      
        } ;
}
